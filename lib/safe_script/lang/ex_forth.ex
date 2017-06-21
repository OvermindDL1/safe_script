defmodule SafeScript.Lang.ExForth do
  @moduledoc """
  """

  use ExSpirit.Parser, text: true
  alias ExSpirit.TreeMap, as: TreeMap

  @behaviour SafeScript.Compiler

  alias SafeScript.Env


  def compile_expressions(input, opts \\ [])
  def compile_expressions(input, opts) do
    case tokanize(opts, input) do
      {:ok, commands} -> {:ok, {:ex_forth, commands}}
      error -> error
    end
  end


  def interpret_forms(form, opts \\ [])
  def interpret_forms({:ex_forth, commands}, opts) do
    env = Env.new()
    env =
      case opts[:words] do
        fun when is_function(fun, 3) ->
          put_in(env.extra[:words], fn(env, word, tokens) ->
            case fun.(env, word) do
              %Env{} = env -> {env, tokens}
              {%Env{} , _tokens} = ret -> ret
              _ -> built_ins(env, word, tokens)
            end
          end)
        _ -> put_in(env.extra[:words], &built_ins/3)
      end
    env =
      case opts[:externals] do
        fun when is_function(fun, 3) -> put_in(env.extra[:externals], fun)
        _ -> env
      end
    env = put_in(env.extra[:script_words], %{})
    env = put_in(env.extra[:empty_stack_value], opts[:empty_stack_value] || :no_value)
    env = interpret_tokens(env, commands)
    case env.stack do
      [result] -> {:ok, {env, result}}
      results -> {:ok, {env, results}}
    end
  end



  ### Interpretation

  def reduction(env, tokens) do
    case env.extra[:reduction_handler] do
      nil -> env
      fun -> fun.(env, tokens)
    end
  end


  def external_call(env, name, arity) do
    with\
      {env, args} when length(args)===arity <- stack_pop(env, arity),
      fun when is_function(fun, 3) <- env.extra[:externals]
    do
      case fun.(env, name, args) do
        {%Env{} = env, result} ->
          stack_push(env, result)
        %Env{} = env -> stack_push(env, [{:no_external, name, args}])
        result -> stack_push(env, result)
      end
    else
      {env, args} -> stack_push(env, [{:not_enough_args_on_stack_for, name, arity, args}])
      _nofunction -> stack_push(env, [{:no_externals, name, arity}])
    end
  end


  def word_call(env, word, tokens) do
    env = reduction(env, [word | tokens])
    case env.extra.script_words do
      %{^word => word_tokens} ->
        # interpret_tokens(env, word_tokens)
        {env, word_tokens ++ tokens}
      _ -> env.extra.words.(env, word, tokens)
    end
  end


  def stack_push(env, values) when is_list(values) do
    %{env |
      stack: values ++ env.stack,
    }
  end
  def stack_push(env, value) do
    %{env |
      stack: [value | env.stack],
    }
  end

  def stack_pop(env, count \\ 1) when count>=0 do
    {stack, values} = do_stack_split(env.extra.empty_stack_value, count, env.stack)
    values = :lists.reverse(values)
    {%{env | stack: stack}, values}
  end
  defp do_stack_split(default_value, count, stack, values \\ [])
  defp do_stack_split(_default_value, 0, stack, values),       do: {stack, values}
  defp do_stack_split(default_value, count, [], values),       do: do_stack_split(default_value, count-1, [], [default_value | values])
  defp do_stack_split(default_value, count, [v|rest], values), do: do_stack_split(default_value, count-1, rest, [v | values])


  def interpret_tokens(env, tokens)
  def interpret_tokens(env, []), do: env
  def interpret_tokens(env, [int | rest]) when is_integer(int), do: interpret_tokens(stack_push(env, [int]), rest)
  def interpret_tokens(env, [float | rest]) when is_float(float), do: interpret_tokens(stack_push(env, [float]), rest)
  def interpret_tokens(env, [atom | rest]) when is_atom(atom), do: interpret_tokens(stack_push(env, [atom]), rest)
  def interpret_tokens(env, [string | rest]) when is_binary(string), do: interpret_tokens(stack_push(env, [string]), rest)
  def interpret_tokens(env, [{:external, {name, arity}} | rest]), do: interpret_tokens(external_call(env, name, arity), rest)
  def interpret_tokens(env, [{:word, word} | rest]) do
    {env, rest} = word_call(env, word, rest)
    interpret_tokens(env, rest)
  end
  # def interpret_tokens(env, [unknown_token | rest]), do: throw {:ExForth, :unknown_token, unknown_token, rest, env}
  def interpret_tokens(_env, tokens), do: throw {:ExForth, :invalid_tokens, tokens}



  def built_ins(env, word, tokens)
  ## Comments
  def built_ins(env, "(", tokens) do
    [_ | tokens] = Enum.drop_while(tokens, fn {:word, ")"} -> false; _ -> true end)
    {env, tokens}
  end
  ## Function definition
  def built_ins(env, ":", tokens) do
    [{:word, name} | tokens] = tokens
    {func_tokens, [_semicolon | tokens]} = Enum.split_while(tokens, fn {:word, ";"} -> false; _ -> true end)
    env = put_in(env.extra.script_words[name], func_tokens)
    {env, tokens}
  end
  ## Numbers
  def built_ins(env, "+", tokens) do
    {env, [t0, t1]} = stack_pop(env, 2)
    env = stack_push(env, [t0+t1])
    {env, tokens}
  end
  def built_ins(env, "-", tokens) do
    {env, [t0, t1]} = stack_pop(env, 2)
    env = stack_push(env, [t0-t1])
    {env, tokens}
  end
  def built_ins(env, "*", tokens) do
    {env, [t0, t1]} = stack_pop(env, 2)
    env = stack_push(env, [t0*t1])
    {env, tokens}
  end
  def built_ins(env, "/", tokens) do
    {env, result} =
      case stack_pop(env, 2) do
        {env, [0, _t1]} -> {env, 0}
        {env, [0.0, _t1]} -> {env, 0.0}
        {env, [t0, t1]} when is_integer(t1) and is_integer(t0) -> {env, div(t0, t1)}
        {env, [t0, t1]} -> {env, t0 / t1}
      end
    env = stack_push(env, [result])
    {env, tokens}
  end
  ## Basic forth calls
  def built_ins(env, "dup", tokens) do
    {env, [a]} = stack_pop(env)
    env = stack_push(env, [a, a])
    {env, tokens}
  end
  def built_ins(env, "drop", tokens) do
    {env, _} = stack_pop(env)
    {env, tokens}
  end
  def built_ins(env, "swap", tokens) do
    {env, [t0, t1]} = stack_pop(env, 2)
    env = stack_push(env, [t1, t0])
    {env, tokens}
  end
  def built_ins(env, "over", tokens) do
    {env, [t0, t1]} = stack_pop(env, 2)
    env = stack_push(env, [t1, t0, t1])
    {env, tokens}
  end
  def built_ins(env, "rot", tokens) do
    {env, [t0, t1, t2]} = stack_pop(env, 2)
    env = stack_push(env, [t2, t0, t1])
    {env, tokens}
  end
  def built_ins(env, word, tokens) do
    {stack_push(env, [{:unknown_word, word}]), tokens}
    throw {:unknown_word, word, tokens, env}
  end



  ### Tokanization

  defp tokanize(_opts, input) do
    result =
      input
      |> String.split()
      |> Enum.map(&parse_type_of/1)
      |> List.flatten()
    {:ok, result}
  end


  defp parse_type_of(input) do
    case parse(input, forth_parser()) do
      %{error: nil, result: result, rest: ""} -> result
      %{error: error} -> throw {:ExForth, :parse_error, error}
    end
  end



  ### Parser definition

  defp forth_parser_integer_branches, do: %{
    ?b => &uint(&1, 2),
    ?o => &uint(&1, 8),
    ?d => &uint(&1, 10),
    ?x => &uint(&1, 16),
  }

  defrule forth_parser_integer(alt([
    seq([ lit(?0) |> branch(char(), forth_parser_integer_branches()) ]),
    uint(),
  ]))

  defrule forth_parser_float(context) do
    case Float.parse(context.rest) do
      {float, ""} ->
        %{context |
          result: float,
          rest: "",
        }
      _ ->
        %{context |
          error: %ExSpirit.Parser.ParseException{message: "Parsing a floating point number failed", context: context},
        }
    end
  end

  defrule nonquote_char_or_escaped_quote(alt([
    lit(?\\) |> char(?"),
    char(-?"),
  ]))

  elixir_operators = # https://hexdocs.pm/elixir/master/operators.html
    TreeMap.new()
    |> TreeMap.add_text("@", :@)
    |> TreeMap.add_text(".", :.)
    |> TreeMap.add_text("+", :+)
    |> TreeMap.add_text("-", :-)
    |> TreeMap.add_text("!", :!)
    |> TreeMap.add_text("^", :^)
    # |> TreeMap.add_text("not", :not), # Leave out, valid atom anyway
    |> TreeMap.add_text("~~~", :~~~)
    |> TreeMap.add_text("*", :*)
    |> TreeMap.add_text("/", :/)
    |> TreeMap.add_text("++", :++)
    |> TreeMap.add_text("--", :--)
    |> TreeMap.add_text("..", :..)
    |> TreeMap.add_text("<>", :<>)
    # |> TreeMap.add_text("in", :in), # Leave out, valid atom anyway
    # |> TreeMap.add_text("not in", :"not in"), # Leave out, valid atom anyway
    |> TreeMap.add_text("|>", :|>)
    |> TreeMap.add_text("<<<", :<<<)
    |> TreeMap.add_text(">>>", :>>>)
    |> TreeMap.add_text("~>>", :~>>)
    |> TreeMap.add_text("<<~", :<<~)
    |> TreeMap.add_text("~>", :~>)
    |> TreeMap.add_text("<~", :<~)
    |> TreeMap.add_text("<~>", :<~>)
    |> TreeMap.add_text("<|>", :<|>)
    |> TreeMap.add_text("<", :<)
    |> TreeMap.add_text(">", :>)
    |> TreeMap.add_text("<=", :<=)
    |> TreeMap.add_text(">=", :>=)
    |> TreeMap.add_text("==", :==)
    |> TreeMap.add_text("!=", :!=)
    |> TreeMap.add_text("=~", :=~)
    |> TreeMap.add_text("===", :===)
    |> TreeMap.add_text("!==", :!==)
    |> TreeMap.add_text("&&", :&&)
    |> TreeMap.add_text("&&&", :&&&)
    # |> TreeMap.add_text("and", :and), # Leave out, valid atom anyway
    |> TreeMap.add_text("||", :||)
    |> TreeMap.add_text("|||", :|||)
    # |> TreeMap.add_text("or", :or), # Leave out, valid atom anyway
    |> TreeMap.add_text("=", :=)
    |> TreeMap.add_text("=>", :"=>") # Wtf?  Why is :=> not a valid atom even though all elixir operators with `:` at the front should be valid atoms??
    |> TreeMap.add_text("|", :|)
    |> TreeMap.add_text("::", :::)
    # |> TreeMap.add_text("when", :when), # Leave out, valid atom anyway
    |> TreeMap.add_text("<-", :<-)
    |> TreeMap.add_text("\\", :\\)
    |> TreeMap.add_text("&", :&)

  defrule forth_parser_atom_string(alt([
    seq([ lit(?"), repeat(nonquote_char_or_escaped_quote(), 1), lit(?")]),
    chars1([?A..?Z, ?a..?z, ?_], [?A..?Z, ?a..?z, ?_, ?0..?9]),
  ])), pipe_result_into: :erlang.iolist_to_binary()

  defrule forth_parser_atom(lit(?:) |> alt([
    forth_parser_atom_string() |> pipe_result_into(String.to_existing_atom()),
    symbols(unquote(Macro.escape(elixir_operators))),
  ]))

  defrule forth_parser_string(seq([
    lit(?"), repeat(nonquote_char_or_escaped_quote(), 1), lit(?"),
  ])), pipe_result_into: :erlang.iolist_to_binary()

  defrule forth_parser_external_call(seq([
    alt([
      seq([ forth_parser_atom_string(), repeat(seq([lit(?.), forth_parser_atom_string()]), 1) ]),
      forth_parser_atom(),
      forth_parser_atom_string(),
      forth_parser_string()
      ]),
    lit(?/),
    uint()
    ])), pipe_result_into: (case do
      [names, arity] when is_list(names) ->
        names = Enum.map(names, &String.to_existing_atom/1)
        {name, modules} = List.pop_at(names, -1)
        {{modules, name}, arity}
      [name, arity] -> {String.to_existing_atom(name), arity}
    end)

  defrule forth_parser(alt([
    lit(?-) |> forth_parser_integer() |> eoi(pass_result: true) |> pipe_result_into(Kernel.-()),
    forth_parser_integer() |> eoi(pass_result: true),
    forth_parser_float() |> eoi(pass_result: true),
    forth_parser_atom() |> eoi(pass_result: true),
    forth_parser_string() |> eoi(pass_result: true),
    tag(:external, forth_parser_external_call()) |> eoi(pass_result: true),
    tag(:word, chars(-1)),
  ]))
end
