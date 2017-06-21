defmodule SafeScriptTest.ExForth do
  use ExUnit.Case, async: true
  doctest SafeScript.Lang.ExForth


  def lang(), do: ExForth


  test "Integers" do
    assert {:ok, {_env, 4}} = SafeScript.eval_expressions("2 2 +", lang: lang())
    assert {:ok, {_env, 0}} = SafeScript.eval_expressions("2 2 -", lang: lang())
    assert {:ok, {_env, 4}} = SafeScript.eval_expressions("2 2 *", lang: lang())
    assert {:ok, {_env, 1}} = SafeScript.eval_expressions("2 2 /", lang: lang())
    assert {:ok, {_env, 21}} = SafeScript.eval_expressions("0 1 2 0b11 0d4 0o5 0x6 + + + + + +", lang: lang())
  end


  test "Floats" do
    assert {:ok, {_env, 4.0}} = SafeScript.eval_expressions("2.0 2.0 +", lang: lang())
    assert {:ok, {_env, 0.0}} = SafeScript.eval_expressions("2.0 2.0 -", lang: lang())
    assert {:ok, {_env, 4.0}} = SafeScript.eval_expressions("2.0 2.0 *", lang: lang())
    assert {:ok, {_env, 1.0}} = SafeScript.eval_expressions("2.0 2.0 /", lang: lang())
  end


  test "Atoms" do
    assert {:ok, {_env, :test}} = SafeScript.eval_expressions(":test", lang: lang())
    assert {:ok, {_env, :Test}} = SafeScript.eval_expressions(":Test", lang: lang())
    assert {:ok, {_env, :<|>}} = SafeScript.eval_expressions(":<|>", lang: lang())
    assert {:ok, {_env, :"32DBAS@{!#&"}} = SafeScript.eval_expressions(":\"32DBAS@{!#&\"", lang: lang())
  end


  test "Strings" do
    assert {:ok, {_env, "Tester"}} = SafeScript.eval_expressions("\"Tester\"", lang: lang())
  end


  test "external function" do
    assert {:ok, {_env, [{:no_externals, :add, 2}, 2, 1]}} = SafeScript.eval_expressions("1 2 add/2", lang: lang())
    assert {:ok, {_env, [{:no_externals, {[:Testering], :add}, 2}, 2, 1]}} = SafeScript.eval_expressions("1 2 Testering.add/2", lang: lang())
    assert {:ok, {_env, [{:no_externals, {[:Testering, :Bloop], :add}, 2}, 2, 1]}} = SafeScript.eval_expressions("1 2 Testering.Bloop.add/2", lang: lang())
    externals =
      fn
        (_env, :add, [l, r]) -> l + r
        (_env, {[:Testering], :add}, [l, r]) -> l + r
        (_env, {[:Testering, :Bloop], :add}, [l, r]) -> l + r
      end
    assert {:ok, {_env, 3}} = SafeScript.eval_expressions("1 2 add/2", lang: lang(), externals: externals)
    assert {:ok, {_env, 3}} = SafeScript.eval_expressions("1 2 Testering.add/2", lang: lang(), externals: externals)
    assert {:ok, {_env, 3}} = SafeScript.eval_expressions("1 2 Testering.Bloop.add/2", lang: lang(), externals: externals)
  end


  test "Comments" do
    assert {:ok, {_env, 3}} = SafeScript.eval_expressions("""
      1 ( This is a block comment )
      2 + ( Let's add these together )
    """, lang: lang())
  end


  test "Forth function definition" do
    assert {:ok, {_env, []}} = SafeScript.eval_expressions(": *+ ( a b c -- 'a ) * + ;", lang: lang())
  end

  test "Forth function definition and call" do
    assert {:ok, {_env, 5}} = SafeScript.eval_expressions(": *+ ( a b c -- 'a ) * + ; 3 2 1 *+", lang: lang()) 
  end
end
