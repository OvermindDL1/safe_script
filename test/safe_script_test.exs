defmodule SafeScriptTest do
  use ExUnit.Case
  doctest SafeScript


  test "Integer operators" do
    assert {:ok, {_env, 4}} = SafeScript.eval_expressions("2 + 2")
    assert {:ok, {_env, 0}} = SafeScript.eval_expressions("2 - 2")
    assert {:ok, {_env, 4}} = SafeScript.eval_expressions("2 * 2")
    assert {:ok, {_env, 1}} = SafeScript.eval_expressions("2 / 2")
  end


  test "Float operators" do
    assert {:ok, {_env, 4.0}} = SafeScript.eval_expressions("2.0 + 2.0")
    assert {:ok, {_env, 0.0}} = SafeScript.eval_expressions("2.0 - 2.0")
    assert {:ok, {_env, 4.0}} = SafeScript.eval_expressions("2.0 * 2.0")
    assert {:ok, {_env, 1.0}} = SafeScript.eval_expressions("2.0 / 2.0")
  end


  test "Custom functions" do
    assert {:ok, {_env, {:no_call, :a_func, [21]}}} = SafeScript.eval_expressions("a_func(21)")
    op =
      fn
        (_env, :null_func, []) -> 42 # Return a value straight, or a tuple of:  {env, result}
        (_env, :a_func, [arg]) -> arg > 0
        (env, _, _) -> env # Returning just the `env` is saying that 'there is no call here' and will be alerted as such
      end
    assert {:ok, {_env, 42}} = SafeScript.eval_expressions("null_func()", externals: op)
    assert {:ok, {_env, true}} = SafeScript.eval_expressions("a_func(21)", externals: op)
    assert {:ok, {_env, true}} = SafeScript.eval_expressions("a_func(null_func())", externals: op)
    assert {:ok, {_env, {:no_call, :does_not_exist, []}}} = SafeScript.eval_expressions("does_not_exist()", externals: op)
  end


  test "Custom operator - >>>" do
    assert {:ok, {_env, {:no_call, :<|>, [2, 2]}}} = SafeScript.eval_expressions("2 <|> 2")
    op = fn(_env, :<|>, [l, r]) -> l + r end
    assert {:ok, {_env, 4}} = SafeScript.eval_expressions("2 <|> 2", externals: op)
  end


  test "Module call" do
    assert {:ok, {_env, {:no_call, {[:Testering], :a_func}, [21]}}} = SafeScript.eval_expressions("Testering.a_func(21)")
    assert {:ok, {_env, {:no_call, {[:Testering, :Bloop], :a_func}, [21]}}} = SafeScript.eval_expressions("Testering.Bloop.a_func(21)")
    assert {:ok, {_env, {:no_call, {{:no_call, :testering, nil}, :a_func}, [21]}}} = SafeScript.eval_expressions("testering.a_func(21)")
    op =
      fn
        (_env, {[:Testering], :a_func}, [arg]) -> arg > 0
        (_env, {[:Testering, :Bloop], :a_func}, [arg]) -> arg > 0
        (_env, :testering, [x]) when is_integer(x) -> x - 1
      end
    assert {:ok, {_env, true}} = SafeScript.eval_expressions("Testering.a_func(21)", externals: op)
    assert {:ok, {_env, true}} = SafeScript.eval_expressions("Testering.Bloop.a_func(21)", externals: op)
    assert {:ok, {_env, 20}} = SafeScript.eval_expressions("testering(21)", externals: op)
  end


  test "External Binding Lookup" do
    assert {:ok, {_env, {:no_call, :life, nil}}} = SafeScript.eval_expressions("life")
    op =
      fn
        (_env, :life, nil) -> 42
        (env, _, _) -> env
      end
    assert {:ok, {_env, 42}} = SafeScript.eval_expressions("life", externals: op)
  end


  test "External Binding Lookup - call" do
    assert {:ok, {_env, {:no_call, :life, nil}}} = SafeScript.eval_expressions("life")
    op =
      fn
        (_env, :life, nil) -> 42
        (_env, :testering, nil) -> fn x -> x - 2 end
      end
    assert {:ok, {_env, 42}} = SafeScript.eval_expressions("life", externals: op)
    assert {:ok, {_env, 19}} = SafeScript.eval_expressions("testering.(21)", externals: op)
  end
end
