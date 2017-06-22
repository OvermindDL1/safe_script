defmodule SafeScriptTest.ExForth do
  use ExUnit.Case, async: true
  doctest SafeScript.Lang.ExForth


  def lang(), do: ExForth


  test "Parser test - integer" do
    assert {:ok, {:ex_forth, [0]}} = SafeScript.compile_expressions("0", lang: lang())
    assert {:ok, {:ex_forth, [1]}} = SafeScript.compile_expressions("1", lang: lang())
    assert {:ok, {:ex_forth, [-1]}} = SafeScript.compile_expressions("-1", lang: lang())
    assert {:ok, {:ex_forth, [2]}} = SafeScript.compile_expressions("2", lang: lang())
    assert {:ok, {:ex_forth, [-2]}} = SafeScript.compile_expressions("-2", lang: lang())
  end


  test "Parser test - integer - decimal" do
    assert {:ok, {:ex_forth, [0]}} = SafeScript.compile_expressions("0d0", lang: lang())
    assert {:ok, {:ex_forth, [1]}} = SafeScript.compile_expressions("0d1", lang: lang())
    assert {:ok, {:ex_forth, [-1]}} = SafeScript.compile_expressions("-0d1", lang: lang())
    assert {:ok, {:ex_forth, [2]}} = SafeScript.compile_expressions("0d2", lang: lang())
    assert {:ok, {:ex_forth, [-2]}} = SafeScript.compile_expressions("-0d2", lang: lang())
  end


  test "Parser test - integer - hexidecimal" do
    assert {:ok, {:ex_forth, [0]}} = SafeScript.compile_expressions("0x0", lang: lang())
    assert {:ok, {:ex_forth, [1]}} = SafeScript.compile_expressions("0x1", lang: lang())
    assert {:ok, {:ex_forth, [-1]}} = SafeScript.compile_expressions("-0x1", lang: lang())
    assert {:ok, {:ex_forth, [2]}} = SafeScript.compile_expressions("0x2", lang: lang())
    assert {:ok, {:ex_forth, [-2]}} = SafeScript.compile_expressions("-0x2", lang: lang())
    assert {:ok, {:ex_forth, [15]}} = SafeScript.compile_expressions("0xf", lang: lang())
    assert {:ok, {:ex_forth, [-15]}} = SafeScript.compile_expressions("-0xf", lang: lang())
    assert {:ok, {:ex_forth, [255]}} = SafeScript.compile_expressions("0xFf", lang: lang())
    assert {:ok, {:ex_forth, [-255]}} = SafeScript.compile_expressions("-0xFf", lang: lang())
  end


  test "Parser test - integer - octal" do
    assert {:ok, {:ex_forth, [0]}} = SafeScript.compile_expressions("0o0", lang: lang())
    assert {:ok, {:ex_forth, [1]}} = SafeScript.compile_expressions("0o1", lang: lang())
    assert {:ok, {:ex_forth, [-1]}} = SafeScript.compile_expressions("-0o1", lang: lang())
    assert {:ok, {:ex_forth, [2]}} = SafeScript.compile_expressions("0o2", lang: lang())
    assert {:ok, {:ex_forth, [-2]}} = SafeScript.compile_expressions("-0o2", lang: lang())
    assert {:ok, {:ex_forth, [7]}} = SafeScript.compile_expressions("0o7", lang: lang())
    assert {:ok, {:ex_forth, [-7]}} = SafeScript.compile_expressions("-0o7", lang: lang())
    assert {:ok, {:ex_forth, [63]}} = SafeScript.compile_expressions("0o77", lang: lang())
    assert {:ok, {:ex_forth, [-63]}} = SafeScript.compile_expressions("-0o77", lang: lang())
  end


  test "Parser test - integer - binary" do
    assert {:ok, {:ex_forth, [0]}} = SafeScript.compile_expressions("0b0", lang: lang())
    assert {:ok, {:ex_forth, [1]}} = SafeScript.compile_expressions("0b1", lang: lang())
    assert {:ok, {:ex_forth, [-1]}} = SafeScript.compile_expressions("-0b1", lang: lang())
    assert {:ok, {:ex_forth, [2]}} = SafeScript.compile_expressions("0b10", lang: lang())
    assert {:ok, {:ex_forth, [-2]}} = SafeScript.compile_expressions("-0b10", lang: lang())
    assert {:ok, {:ex_forth, [3]}} = SafeScript.compile_expressions("0b11", lang: lang())
    assert {:ok, {:ex_forth, [-3]}} = SafeScript.compile_expressions("-0b11", lang: lang())
    assert {:ok, {:ex_forth, [42]}} = SafeScript.compile_expressions("0b101010", lang: lang())
    assert {:ok, {:ex_forth, [-42]}} = SafeScript.compile_expressions("-0b101010", lang: lang())
  end


  test "Parser test - float" do
    assert {:ok, {:ex_forth, [0.0]}} = SafeScript.compile_expressions("0.0", lang: lang())
    assert {:ok, {:ex_forth, [1.0]}} = SafeScript.compile_expressions("1.0", lang: lang())
    assert {:ok, {:ex_forth, [-1.0]}} = SafeScript.compile_expressions("-1.0", lang: lang())
    assert {:ok, {:ex_forth, [6.28]}} = SafeScript.compile_expressions("6.28", lang: lang())
    assert {:ok, {:ex_forth, [-6.28]}} = SafeScript.compile_expressions("-6.28", lang: lang())
    assert {:ok, {:ex_forth, [628.0]}} = SafeScript.compile_expressions("6.28e2", lang: lang())
    assert {:ok, {:ex_forth, [-628.0]}} = SafeScript.compile_expressions("-6.28e2", lang: lang())
    assert {:ok, {:ex_forth, [0.0628]}} = SafeScript.compile_expressions("6.28e-2", lang: lang())
    assert {:ok, {:ex_forth, [-0.0628]}} = SafeScript.compile_expressions("-6.28e-2", lang: lang())
  end


  test "Parser test - atom" do
    assert {:ok, {:ex_forth, [:ok]}} = SafeScript.compile_expressions(":ok", lang: lang())
    assert {:ok, {:ex_forth, [:ok]}} = SafeScript.compile_expressions(":\"ok\"", lang: lang())
    assert {:ok, {:ex_forth, [:"$ok$"]}} = SafeScript.compile_expressions(":\"$ok$\"", lang: lang())
    assert {:ok, {:ex_forth, [:<|>]}} = SafeScript.compile_expressions(":<|>", lang: lang())
  end


  test "Parser test - atom - nonexisting" do # Atom's that do not exist in the current running system just become words
    assert {:ok, {:ex_forth, [{:word, ":THIS_should_NOT_exist", nil}]}} = SafeScript.compile_expressions(":THIS_should_NOT_exist", lang: lang())
  end


  test "Parser test - external call" do
    assert {:ok, {:ex_forth, [external: {:ok, 2}]}} = SafeScript.compile_expressions("ok/2", lang: lang())
    assert {:ok, {:ex_forth, [external: {{[:Module], :ok}, 2}]}} = SafeScript.compile_expressions("Module.ok/2", lang: lang())
    assert {:ok, {:ex_forth, [external: {{[:Module, :SubModule], :ok}, 2}]}} = SafeScript.compile_expressions("Module.SubModule.ok/2", lang: lang())
    assert {:ok, {:ex_forth, [external: {:ok, 2}]}} = SafeScript.compile_expressions(":ok/2", lang: lang())
    assert {:ok, {:ex_forth, [external: {{[:Module], :ok}, 2}]}} = SafeScript.compile_expressions(":Module.ok/2", lang: lang())
    assert {:ok, {:ex_forth, [external: {{[:Module], :ok}, 2}]}} = SafeScript.compile_expressions("Module.\"ok\"/2", lang: lang())
    assert {:ok, {:ex_forth, [external: {{[:Module], :ok}, 2}]}} = SafeScript.compile_expressions("\"Module\".ok/2", lang: lang())
    assert {:ok, {:ex_forth, [external: {{[:Module], :ok}, 2}]}} = SafeScript.compile_expressions("\"Module\".\"ok\"/2", lang: lang())
    assert {:ok, {:ex_forth, [external: {{[:Module], :ok}, 2}]}} = SafeScript.compile_expressions(":\"Module\".ok/2", lang: lang())
    assert {:ok, {:ex_forth, [external: {{[:Module], :ok}, 2}]}} = SafeScript.compile_expressions(":\"Module\".\"ok\"/2", lang: lang())
    assert {:ok, {:ex_forth, [external: {:<|>, 2}]}} = SafeScript.compile_expressions("<|>/2", lang: lang())
    assert {:ok, {:ex_forth, [external: {{[:Module], :<|>}, 2}]}} = SafeScript.compile_expressions("Module.<|>/2", lang: lang())
    assert {:ok, {:ex_forth, [external: {{[:<|>], :ok}, 2}]}} = SafeScript.compile_expressions("<|>.ok/2", lang: lang())
    assert {:ok, {:ex_forth, [external: {{[:Module, :<|>], :ok}, 2}]}} = SafeScript.compile_expressions("Module.<|>.ok/2", lang: lang())
    # Unknown atoms are left as strings to be passed to the FFI directly
    assert {:ok, {:ex_forth, [external: {"THIS_should_NOT_exist", 2}]}} = SafeScript.compile_expressions("THIS_should_NOT_exist/2", lang: lang())
    assert {:ok, {:ex_forth, [external: {{["THIS_should_NOT_exist"], "nor_THIS"}, 2}]}} = SafeScript.compile_expressions("THIS_should_NOT_exist.nor_THIS/2", lang: lang())
  end


  test "Parser test - string" do
    assert {:ok, {:ex_forth, ["Simple String"]}} = SafeScript.compile_expressions("\"Simple String\"", lang: lang())
    assert {:ok, {:ex_forth, ["Tester \n \r \t \s \" \\ done"]}} = SafeScript.compile_expressions("\"Tester \\n \\r \\t \\s \\\" \\\\ done\"", lang: lang())
    assert {:ok, {:ex_forth, ["Test newline \n return \r tab \t space \s"]}} = SafeScript.compile_expressions("\"Test newline \\n return \\r tab \\t space \\s\"", lang: lang())
  end


  test "Parser test - word" do # Everything else just ends up as a word
    assert {:ok, {:ex_forth, [{:word, "everything_else", nil}]}} = SafeScript.compile_expressions("everything_else", lang: lang())
    assert {:ok, {:ex_forth, [{:word, "@{*&()#", nil}]}} = SafeScript.compile_expressions("@{*&()#", lang: lang())
  end


  test "Integers" do
    assert {:ok, {_env, 4}} = SafeScript.eval_expressions("2 2 +", lang: lang())
    assert {:ok, {_env, -5}} = SafeScript.eval_expressions("0 5 -", lang: lang())
    assert {:ok, {_env, 4}} = SafeScript.eval_expressions("2 2 *", lang: lang())
    assert {:ok, {_env, 1}} = SafeScript.eval_expressions("2 2 /", lang: lang())
    assert {:ok, {_env, 21}} = SafeScript.eval_expressions("0 1 2 0b11 0d4 0o5 0x6 + + + + + +", lang: lang())
  end


  test "Floats" do
    assert {:ok, {_env, 4.0}} = SafeScript.eval_expressions("2.0 2.0 +", lang: lang())
    assert {:ok, {_env, -5.0}} = SafeScript.eval_expressions("0.0 5.0 -", lang: lang())
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


  test "String Escapes" do
    assert {:ok, {_env, "Tester \n \r \t \s \" \\ done"}} = SafeScript.eval_expressions("\"Tester \\n \\r \\t \\s \\\" \\\\ done\"", lang: lang())
    assert {:ok, {_env, "Test newline \n return \r tab \t space \s"}} = SafeScript.eval_expressions("\"Test newline \\n return \\r tab \\t space \\s\"", lang: lang())
  end


  test "External function" do
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


  test "External function - FFI" do
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


  test "Forth function Forgetting" do
    assert {:ok, {_env, [{:unknown_word, "*+"}, 3, 2, 1]}} = SafeScript.eval_expressions(": *+ ( a b c -- 'a ) * + ; ; *+ 1 2 3 *+", lang: lang())
  end


  test "Forth function accessing prior function name" do
    assert {:ok, {_env, ["( a b c -- 'a ) *+ SEE *+", "( a b c -- 'a ) * +", 7]}} = SafeScript.eval_expressions("""
    : *+ ( a b c -- 'a ) * + ;
    : *+ ( a b c -- 'a ) *+ SEE *+ ;
    1 2 3 *+
    SEE *+
    """, lang: lang())
  end


  test "Forth function definition and call" do
    assert {:ok, {_env, 5}} = SafeScript.eval_expressions(": *+ ( a b c -- 'a ) * + ; 3 2 1 *+", lang: lang())
  end


  test "Forth function definition with unquoting" do
    assert {:ok, {_env, 5}} = SafeScript.eval_expressions("QUOTE + : *+ * `UNQUOTE ; 3 2 1 *+", lang: lang())
    assert {:ok, {_env, "* +"}} = SafeScript.eval_expressions("QUOTE + : *+ * `UNQUOTE ; SEE *+", lang: lang())
  end


  test "Unquoting" do
    assert {:ok, {_env, 2}} = SafeScript.eval_expressions("2 UNQUOTE", lang: lang())
    assert {:ok, {_env, {:word, "+", _}}} = SafeScript.eval_expressions("\"+\" WORD", lang: lang())
    assert {:ok, {_env, 3}} = SafeScript.eval_expressions("\"+\" WORD 1 2 ROT UNQUOTE", lang: lang())
    assert {:ok, {_env, 3}} = SafeScript.eval_expressions("QUOTE + 1 2 ROT UNQUOTE", lang: lang())
    assert {:ok, {_env, 3}} = SafeScript.eval_expressions("QUOTE + : + - ; 2 1 + 2 ROT UNQUOTE", lang: lang())
  end


  test "Quoting" do
    assert {:ok, {_env, {:word, "+", _}}} = SafeScript.eval_expressions("QUOTE +", lang: lang())
    assert {:ok, {_env, {:external, {:add, 2}}}} = SafeScript.eval_expressions("QUOTE add/2", lang: lang())
    assert {:ok, {_env, 2}} = SafeScript.eval_expressions("QUOTE 2", lang: lang())
  end


  test "Forth command - DUP" do
    assert {:ok, {_env, [:no_value, :no_value]}} = SafeScript.eval_expressions("DUP", lang: lang())
    assert {:ok, {_env, [2, 2]}} = SafeScript.eval_expressions("2 DUP", lang: lang())
  end


  test "Forth command - SWAP" do
    assert {:ok, {_env, [:no_value, :no_value]}} = SafeScript.eval_expressions("SWAP", lang: lang())
    assert {:ok, {_env, [2, 1]}} = SafeScript.eval_expressions("2 1 SWAP", lang: lang())
    assert {:ok, {_env, [2, 1, 1, 2]}} = SafeScript.eval_expressions("2 1 2 1 SWAP", lang: lang())
  end


  test "Forth command - OVER" do
    assert {:ok, {_env, [:no_value, :no_value, :no_value]}} = SafeScript.eval_expressions("OVER", lang: lang())
    assert {:ok, {_env, [1, 2, 1]}} = SafeScript.eval_expressions("1 2 OVER", lang: lang())
  end


  test "Forth command - ROT" do
    assert {:ok, {_env, [:no_value, :no_value, :no_value]}} = SafeScript.eval_expressions("ROT", lang: lang())
    assert {:ok, {_env, [1, 3, 2]}} = SafeScript.eval_expressions("1 2 3 ROT ", lang: lang())
  end


  test "Forth command - SEE" do
    assert {:ok, {_env, "( WORD `*+` IS NOT DEFINED )"}} = SafeScript.eval_expressions("SEE *+", lang: lang())
    assert {:ok, {_env, "( NATIVE WORD `SEE` )"}} = SafeScript.eval_expressions("SEE SEE", lang: lang())
    assert {:ok, {_env, "( a b c -- 'a ) * +"}} = SafeScript.eval_expressions(": *+   ( a b c   -- 'a    ) *    + ; SEE *+", lang: lang())
  end


  test "Case sensitivity or insensitivity" do
    assert {:ok, {_env, 3}} = SafeScript.eval_expressions(": add ( a b -- 'a ) + ; 1 2 add", lang: lang())
    assert {:ok, {_env, [{:unknown_word, "add"}, 2, 1]}} = SafeScript.eval_expressions(": AdD ( a b -- 'a ) + ; 1 2 add", lang: lang())
    assert {:ok, {_env, 3}} = SafeScript.eval_expressions(": add ( a b -- 'a ) + ; 1 2 add", lang: lang(), always_uppercase_words: true)
    assert {:ok, {_env, 3}} = SafeScript.eval_expressions(": AdD ( a b -- 'a ) + ; 1 2 add", lang: lang(), always_uppercase_words: true)
    assert {:ok, {_env, "( A B -- 'A ) +"}} = SafeScript.eval_expressions(": add   ( a b   -- 'a    )   + ; SEE add", lang: lang(), always_uppercase_words: true)
  end


  test "List operations" do
    assert {:ok, {_env, []}} = SafeScript.eval_expressions("[]", lang: lang())
    assert {:ok, {_env, [3, 2, 1, 0]}} = SafeScript.eval_expressions("3 2 1 0 [] :: :: :: ::", lang: lang())
    assert {:ok, {_env, [3, 2, 1, 0]}} = SafeScript.eval_expressions("3 2 1 0 [] 4 ::N", lang: lang())
    assert {:ok, {_env, []}} = SafeScript.eval_expressions("0 []N", lang: lang())
    assert {:ok, {_env, [0, 1]}} = SafeScript.eval_expressions("0 1 2 []N", lang: lang())
    # assert {:ok, {_env, [0, 1]}} = SafeScript.eval_expressions(": add1 1 + ; QUOTE + 0 1 2 []N MAP", lang: lang())
  end
end
