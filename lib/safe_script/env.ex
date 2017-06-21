defmodule SafeScript.Env do
  @moduledoc """
  This holds the SafeScript environment for bindings, stack, and other language specific data
  """

  @type t :: %SafeScript.Env{
    bindings: list(map()),
    stack: list(any()),
    extra: map(),
  }

  defstruct bindings: [%{}], stack: [], extra: %{}

  def new(), do: %__MODULE__{}

  def map(lst, env, fun)
  def map([], env, _fun), do: {env, []}
  def map([v | rest], env, fun) do
    {env, result} = fun.(env, v)
    {env, rest} = map(rest, env, fun)
    {env, [result | rest]}
  end

  def fold_left(lst, env, fun)
  def fold_left([], env, _fun), do: env
  def fold_left([v | rest], env, fun) do
    fold_left(rest, fun.(env, v), fun)
  end
end
