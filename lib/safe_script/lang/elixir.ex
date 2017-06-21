defmodule SafeScript.Lang.Elixir do
  @moduledoc """
  """

  @behaviour SafeScript.Compiler

  alias SafeScript.Env


  def compile_expressions(input, opts \\ []) do
    opts = Keyword.put_new(opts, :existing_atoms_only, true)
    case Code.string_to_quoted(input, opts) do
      {:ok, ast} -> {:ok, {:elixir_ast, ast}}
      {:error, _err} = error -> error
    end
  end


  def interpret_forms(form, opts \\ [])
  def interpret_forms({:elixir_ast, ast}, opts) do
    interpret_forms({:elixir_cont, Env.new(), ast}, opts)
  end
  def interpret_forms({:elixir_cont, env, ast}, opts) do
    env =
      case opts[:call] do
        fun when is_function(fun, 3) -> put_in(env.extra[:call], fun) # fun returns {env, result}
        nil ->
          case opts[:externals] do
            fun when is_function(fun, 3) ->
              put_in(env.extra[:call], fn(env, name, args) ->
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
            nil -> put_in(env.extra[:call], fn(env, name, args) ->
                case SafeScript.default_calls(env, name, args) do
                  {_env, _result}=returned -> returned
                  call_not_found -> {env, call_not_found}
                end
              end) # fn(_env, name, args) -> throw {:no_call, name, args} end)
          end
      end
    {:ok, interpret_forms_(env, ast)}
  end


  defp call(env, name, args)
  defp call(%{extra: %{call: call}}=env, name, args), do: call.(env, name, args)
  defp call(_env, name, args), do: throw {:no_call, name, args}


  defp interpret_forms_(env, form)
  defp interpret_forms_(env, int) when is_integer(int),    do: {env, int}
  defp interpret_forms_(env, float) when is_float(float),  do: {env, float}
  defp interpret_forms_(env, atom) when is_atom(atom),     do: {env, atom}
  defp interpret_forms_(env, bin) when is_binary(bin),     do: {env, bin}
  defp interpret_forms_(env, {binding, _meta, scope}) when is_atom(binding) and is_atom(scope) do
    case env.bindings do
      [%{^binding => value} | _] -> {env, value}
      _ -> call(env, binding, scope)
    end
  end
  defp interpret_forms_(env, {:__aliases__, _meta, args}) when is_list(args) do
    Env.map(args, env, &interpret_forms_/2)
  end
  defp interpret_forms_(env, {:., _meta, [callee]}) do
    # {env, callee} = interpret_forms_(env, callee)
    # case callee do
    #   callee when is_function(callee) -> {env, callee}
    # end
    interpret_forms_(env, callee)
  end
  defp interpret_forms_(env, {:., _meta, [callee, call]}) do
    {env, callee} = interpret_forms_(env, callee)
    {env, call} = interpret_forms_(env, call)
    {env, {callee, call}}
  end
  defp interpret_forms_(env, {fun, _meta, args}) when is_list(args) do
    {env, fun_name} = interpret_forms_(env, fun)
    {env, args} = Env.map(args, env, &interpret_forms_/2)
    case fun_name do
      fun when is_function(fun, length(args)) -> {env, apply(fun, args)}
      fun_name -> call(env, fun_name, args)
    end
  end
  defp interpret_forms_(env, unknown), do: throw {:unhandled_ast, unknown, env}
end
