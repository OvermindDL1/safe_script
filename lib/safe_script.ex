defmodule SafeScript do
  @moduledoc """
  Documentation for SafeScript.
  """

  defmodule Env do
    defstruct bindings: [%{}], stack: [], user: %{}

    def new(), do: %__MODULE__{}

    def map(lst, env, fun)
    def map([], env, _fun), do: {env, []}
    def map([v | rest], env, fun) do
      {env, result} = fun.(env, v)
      {env, rest} = map(rest, env, fun)
      {env, [result | rest]}
    end
  end

  defmodule Elixir do
    def compile(input, opts \\ []) do
      opts = Keyword.put_new(opts, :existing_atoms_only, true)
      case Code.string_to_quoted(input, opts) do
        {:ok, ast} -> {:ok, {:elixir_ast, ast}}
      end
    end


    def interpret_expressions(form, opts \\ [])
    def interpret_expressions({:elixir_ast, ast}, opts) do
      interpret_expressions({:elixir_cont, Env.new(), ast}, opts)
    end
    def interpret_expressions({:elixir_cont, env, ast}, opts) do
      env =
        case opts[:call] do
          fun when is_function(fun, 3) -> put_in(env.user[:call], fun) # fun returns {env, result}
          nil ->
            case opts[:externals] do
              fun when is_function(fun, 3) ->
                put_in(env.user[:call], fn(env, name, args) ->
                  case SafeScript.default_calls(env, name, args) do
                    {_env, _result}=returned -> returned
                    call_not_found ->
                      case fun.(env, name, args) do
                        {%Env{}, _result}=returned -> returned
                        %Env{} -> {env, call_not_found}
                        result -> {env, result}
                      end
                  end
                end)
              nil -> put_in(env.user[:call], fn(env, name, args) ->
                  case SafeScript.default_calls(env, name, args) do
                    {_env, _result}=returned -> returned
                    call_not_found -> {env, call_not_found}
                  end
                end) # fn(_env, name, args) -> throw {:no_call, name, args} end)
            end
        end
      {:ok, interpret_expressions_(env, ast)}
    end


    defp call(env, name, args)
    defp call(%{user: %{call: call}}=env, name, args), do: call.(env, name, args)
    defp call(_env, name, args), do: throw {:no_call, name, args}


    defp interpret_expressions_(env, form)
    defp interpret_expressions_(env, int) when is_integer(int),    do: {env, int}
    defp interpret_expressions_(env, float) when is_float(float),  do: {env, float}
    defp interpret_expressions_(env, atom) when is_atom(atom),     do: {env, atom}
    defp interpret_expressions_(env, {binding, _meta, scope}) when is_atom(binding) and is_atom(scope) do
      case env.bindings do
        [%{^binding => value} | _] -> {env, value}
        _ -> call(env, binding, scope)
      end
    end
    defp interpret_expressions_(env, {:__aliases__, _meta, args}) when is_list(args) do
      Env.map(args, env, &interpret_expressions_/2)
    end
    defp interpret_expressions_(env, {:., _meta, [callee]}) do
      {env, callee} = interpret_expressions_(env, callee)
      case callee do
        callee when is_function(callee) -> {env, callee}
      end
    end
    defp interpret_expressions_(env, {:., _meta, [callee, call]}) do
      {env, callee} = interpret_expressions_(env, callee)
      {env, call} = interpret_expressions_(env, call)
      {env, {callee, call}}
    end
    defp interpret_expressions_(env, {fun, _meta, args}) when is_list(args) do
      {env, fun_name} = interpret_expressions_(env, fun)
      {env, args} = Env.map(args, env, &interpret_expressions_/2)
      case fun_name do
        fun when is_function(fun, length(args)) -> {env, apply(fun, args)}
        fun_name -> call(env, fun_name, args)
      end
    end
    defp interpret_expressions_(env, unknown), do: throw {:unhandled_ast, unknown, env}
  end


  @doc """
  Compile a string into the interpretable form

  ## Examples

      iex> {:ok, {_env, result}} = SafeScript.eval_expressions("2 + 2")
      iex> result
      4

      iex> {:ok, {_env, result}} = SafeScript.eval_expressions("2 - 2")
      iex> result
      0

      iex> {:ok, {_env, result}} = SafeScript.eval_expressions("2 * 2")
      iex> result
      4

      iex> {:ok, {_env, result}} = SafeScript.eval_expressions("2 / 2")
      iex> result
      1

  """
  def eval_expressions(input, opts \\ []) do
    with\
      {:ok, form} <- compile(input, opts),
      do: interpret_expressions(form, opts)
  end


  @doc """
  Compile a string into the interpretable form

  ## Examples

      iex> {succ, _form} = SafeScript.compile("2 + 2")
      iex> succ
      :ok

  """
  def compile(input, opts \\ []) do
    compiler = opts[:compiler] || SafeScript.Elixir
    compiler.compile(input, opts)
  end


  @doc """
  Interprets a complete expression.

  ## Examples

      iex> {:ok, form} = SafeScript.compile("2")
      iex> {:ok, {_env, result}} = SafeScript.interpret_expressions(form)
      iex> result
      2

  """
  def interpret_expressions(form, opts \\ []) do
    compiler = opts[:compiler] || SafeScript.Elixir
    compiler.interpret_expressions(form, opts)
  end



  def default_calls(env, name, args)
  def default_calls(env, :+, [l, r]) when is_integer(l) and is_integer(r),              do: {env, l + r}
  def default_calls(env, :+, [l, r]) when is_float(l) and is_float(r),                  do: {env, l + r}
  def default_calls(env, :-, [l, r]) when is_integer(l) and is_integer(r),              do: {env, l - r}
  def default_calls(env, :-, [l, r]) when is_float(l) and is_float(r),                  do: {env, l - r}
  def default_calls(env, :*, [l, r]) when is_integer(l) and is_integer(r),              do: {env, l * r}
  def default_calls(env, :*, [l, r]) when is_float(l) and is_float(r),                  do: {env, l * r}
  def default_calls(env, :/, [l, r]) when is_integer(l) and is_integer(r) and r !== 0,  do: {env, div(l, r)}
  def default_calls(env, :/, [l, r]) when is_float(l) and is_float(r) and r !== 0.0,    do: {env, l / r}
  def default_calls(_env, name, args), do: {:no_call, name, args}






  # TODO: Perhaps delete the rest, changing how this works...


  @doc """
  Eval a string input straight.

  ## Examples

      iex> SafeScript.safe_eval_of_input("2 + 2")
      {:ok, 4, []}

      iex> SafeScript.safe_eval_of_input("a = 42; 2 + 2")
      {:ok, 4, [a: 42]}

      iex> {:ok, result, _bindings} = SafeScript.safe_eval_of_input("a_fun = fn x -> x * 2 end; a_fun.(21)")
      iex> result
      42

  """
  def safe_eval_of_input(input, binding \\ [], opts \\ [], type \\ :Elixir) do
    with\
      {:ok, ast} <- safe_ast_of_input(input, opts, type),
      result <- safe_eval_of_safe_ast(ast, binding, opts),
      do: result
  end


  @doc """
  Compile a string input straight into a compiled module.

  ## Examples

      iex> SafeScript.safe_compile_of_input("2 + 2")
      {:ok, []}

      # iex> {:ok, [{Testering, _}]} = SafeScript.safe_compile_of_input("defmodule Testering do def a, do: 42 end")
      # iex> :ok
      # :ok

      # iex> SafeScript.safe_compile_of_input("a = 42; 2 + 2")
      # {:ok, 4, [a: 42]}
      #
      # iex> SafeScript.safe_compile_of_input("a_fun = fn x -> x * 2 end; a_fun.(21)")
      # {:ok, 4, [a: 42]}

  """
  def safe_compile_of_input(input, opts \\ [], type \\ :Elixir) do
    with\
      {:ok, ast} <- safe_ast_of_input(input, opts, type),
      {:ok, result} <- safe_compile_of_safe_ast(ast),
      do: {:ok, result}
  end


  @doc """
  Compile a string input straight into a compiled function ready for immediate use

  ## Examples

      iex> # Make sure old versions stick around...
      iex> {:ok, fun0} = SafeScript.safe_compile_of_input_block("2 + 2")
      iex> is_function(fun0, 0)
      true
      iex> fun0.()
      4
      iex> {:ok, fun1} = SafeScript.safe_compile_of_input_block("3 + 3")
      iex> is_function(fun1, 0)
      true
      iex> fun1.()
      6
      iex> fun0.()
      4
      iex> {:ok, fun2} = SafeScript.safe_compile_of_input_block("4 + 4")
      iex> is_function(fun2, 0)
      true
      iex> fun2.()
      8
      iex> fun1.()
      6
      iex> fun0.()
      4
      iex> {:ok, fun3} = SafeScript.safe_compile_of_input_block("5 + 5")
      iex> is_function(fun3, 0)
      true
      iex> fun3.()
      10
      iex> fun2.()
      8
      iex> fun1.()
      6
      iex> fun0.()
      4
      iex> {fun0.(), fun1.(), fun2.(), fun3.()}
      {4, 6, 8, 10}

      iex> {:ok, fun} = SafeScript.safe_compile_of_input_block("a = 42; 2 + 2")
      iex> fun.()
      4

      iex> {:ok, fun} = SafeScript.safe_compile_of_input_block("a_fun = fn x -> x * 2 end; a_fun.(21)")
      iex> fun.()
      42

      iex> {:ok, fun} = SafeScript.safe_compile_of_input_block("a * 2", [:a])
      iex> fun.(21)
      42

      # iex> {:error, %CompileError{}} = SafeScript.safe_compile_of_input_block("a * 2")
      # iex> :ok
      # :ok

      iex> {:ok, fun} = SafeScript.safe_compile_of_input_block("a * 2", [], [a: 21])
      iex> fun.()
      42

      iex> {:ok, fun} = SafeScript.safe_compile_of_input_block("a * b", [:a], [b: 21])
      iex> fun.(2)
      42

      iex> {:error, unsupported_ast} = SafeScript.safe_compile_of_input_block("IO.inspect(42)")
      iex> unsupported_ast
      {:__aliases__, [counter: 0, line: 1], [:IO]}

      # iex> whitelist =
      # ...>   fn
      # ...>     ({:__aliases__, [counter: 0, line: 1], [:IO]}=ast, succ) -> {ast, succ} # Allow the module
      # ...>     ({{:., _, [{:__aliases__, aMeta, [:IO]}, :inspect]}, _, [_]}=ast, succ) -> if(aMeta[:alias], do: {ast, {false, ast}}, else: {ast, succ}) # But only allow certain calls on it
      # ...>     ({{:., _, [{:__aliases__, _, [:IO]}, _]}, _, [_]}=ast, _) -> {ast, {false, ast}} # And not the rest
      # ...>     (ast, v) -> SafeScript.default_safe(ast, v)
      # ...>   end
      # iex> {:ok, fun} = SafeScript.safe_compile_of_input_block("IO.inspect(42)", [], [], [is_allowed_fun: whitelist])
      # iex> fun.()
      # 42

      # Infinite-loop!
      # iex> {:ok, fun} = SafeScript.safe_compile_of_input_block("a_fun = fn f -> f.(f) end; a_fun.(a_fun)")
      # iex> fun.()
      # no_return

      # iex> {:ok, fun} = SafeScript.safe_compile_of_input_block("apply(:'Elixir.IO', :inspect, [42])", [], [], requires: [])
      # iex> fun.()
      # {:__aliases__, [counter: 0, line: 1], [:IO]}

  """
  def safe_compile_of_input_block(input, arg_names \\ [], binding \\ [], opts \\ [], type \\ :Elixir) do
    arguments = Enum.map(arg_names, &Macro.var(&1, nil))
    with\
      {:ok, {:safe, safe_ast}} <- safe_ast_of_input(input, opts, type),
      ast = {:safe, {:fn, [], [{:->, [], [arguments, safe_ast]}]}},
      # {:ok, {:ok, fun, []}} when is_function(fun, 0) <- safe_eval_of_safe_ast(ast, binding, opts),
      {:ok, fun, _} when is_function(fun) <- safe_eval_of_safe_ast(ast, binding, opts),
      do: {:ok, fun}
  end


  @doc """
  Get the Elixir AST form an input, by default Elixir syntax, can support others as well.

  ## Examples

      iex> SafeScript.safe_ast_of_input("2 + 2")
      {:ok, {:safe, {:+, [line: 1], [2, 2]}}}

      iex> SafeScript.safe_ast_of_input("a + 2")
      {:ok, {:safe, {:+, [line: 1], [{:a, [line: 1], nil}, 2]}}}

  """
  def safe_ast_of_input(input, opts \\ [], type \\ :Elixir)
  def safe_ast_of_input(input, opts, :Elixir) when is_binary(input) do
    case Code.string_to_quoted(input) do
      {:error, _reason} = err -> err
      {:ok, ast} ->
        safe_ast_of_ast(ast, opts[:is_allowed_fun] || &default_safe/2)
    end
  end


  def default_safe(ast, acc)
  def default_safe(ast, {false, _}=err), do: {ast, err}
  def default_safe({_, _}=ast, succ), do: {ast, succ}
  def default_safe({:__block__, _, _}=ast, succ), do: {ast, succ}
  def default_safe({:__aliases__, _, _}=ast, _), do: {ast, {false, ast}}
  def default_safe({binding, _, nil}=ast, succ) when is_atom(binding), do: {ast, succ}
  def default_safe({{_, _, _}, _, args}=ast, succ) when is_list(args), do: {ast, succ}
  def default_safe({op, _, _}=ast, succ) when op in [:+, :-, :/, :*, :=, :->, :., :fn], do: {ast, succ}
  def default_safe({local_call, _, args}=ast, succ) when is_atom(local_call) and is_list(args), do: {ast, succ}
  def default_safe(ast, succ)
    when is_integer(ast)
    or is_float(ast)
    or is_binary(ast)
    or is_atom(ast), do: {ast, succ}
  def default_safe(lst, succ) when is_list(lst), do: {lst, succ}
  def default_safe(ast, _), do: {ast, {false, ast}}


  def safe_ast_of_ast(ast, is_allowed_fun \\ &default_safe/2) do
    case Macro.prewalk(ast, {true, %{}}, is_allowed_fun) do
      {ast, {true, _}} -> {:ok, {:safe, ast}}
      {_ast, {false, reason}} -> {:error, reason}
    end

  end


  def safe_eval_of_safe_ast({:safe, ast}, binding \\ [], opts \\ []) do
    try do
      {result, bindings} = Code.eval_quoted(ast, binding, opts)
      {:ok, result, bindings}
    catch
      :error, err -> {:error, err}
      err, reason -> {:error, {err, reason}}
    end
  end


  def safe_compile_of_safe_ast({:safe, ast}) do
    result = Code.compile_quoted(ast)
    {:ok, result}
  end
end
