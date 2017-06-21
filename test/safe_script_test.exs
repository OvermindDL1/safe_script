defmodule SafeScriptTest do
  use ExUnit.Case, async: true
  doctest SafeScript


  # Default language is Elixir
  # def lang(), do: Elixir


  test "eval_expressions" do
    assert {:ok, {_env, 4}} = SafeScript.eval_expressions("2 + 2")
    assert {:ok, {_env, 0}} = SafeScript.eval_expressions("2 - 2")
    assert {:ok, {_env, 4}} = SafeScript.eval_expressions("2 * 2")
    assert {:ok, {_env, 1}} = SafeScript.eval_expressions("2 / 2")
  end


  test "compile_expressions" do
    assert {:ok, {:elixir_ast, {:+, [line: 1], [2, 2]}}} = SafeScript.compile_expressions("2 + 2")
    assert {:ok, {:elixir_ast, {:-, [line: 1], [2, 2]}}} = SafeScript.compile_expressions("2 - 2")
    assert {:ok, {:elixir_ast, {:*, [line: 1], [2, 2]}}} = SafeScript.compile_expressions("2 * 2")
    assert {:ok, {:elixir_ast, {:/, [line: 1], [2, 2]}}} = SafeScript.compile_expressions("2 / 2")
  end
end
