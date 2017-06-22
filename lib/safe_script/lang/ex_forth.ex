defmodule SafeScript.Lang.ExForth do
  @moduledoc """
  This is a variation of FORTH that supports many of Elixir's types while being a Functional Forth.

  ## Language syntax extensions over more traditional FORTHs:

  In addition to the traditional WORDs, which anything becomes if not one of the following,
  it also supports these special syntax extensions

  * Atoms:  `:someatom` or `:"some atom"` or `:"some\"atom"`
  * Strings: `"A string"` or `"A string \" with a quote"`
  * Integers:  No more needing to change modes:
    * Zero regardless of mode:  `0`
    * Decimal:  `42` or `0d42`
    * Hex:  `0xFF` or `0xff`
    * Octol:  `0o774`
    * Binary: `0b100101011`
  * Floats:  `3.14` or `2.68e4`
  * External Calls:  These are specified as a word with a trailing `/{arity}` where arity is an integer
    These will pop off the specified arity off the stack and call the function with those as args in recerse order.
    These do not work with `words`, only with external bound functions
    * Simple Call:  `1 2 add/2`
    * Module Call:  `1 2 SomeModule.EvenSub.Modules.add/2` or `1 2 somemodule.add/2`
    * Atom Call:  `1 2 :add/2`
    * Module Atom Call:  `1 2 :Elixir.Module.Name.add/2`
    * String Call:  `1 2 "Some odd name here"/2`
    * Atom-string Call:  `1 2 :"Some odd name here"/2`
    * Module Atom-string Call:  `1 2 "Module Names"."Even Dotted".Interspersed."Too"."add"/2`

  Within any double quotes everything appears as verbatum except `\\`, which takes the next character as verbatum, thus
  making `"\\""` become the string `"` and `"\\\\"` become the string `\\` except when the next character is a special
  character code of the following list:

  * `\\n`: Newline
  * `\\r`: Return
  * `\\t`: Tab
  * `\\s`: Space
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
    # env =
    #   case opts[:words] do
    #     fun when is_function(fun, 3) ->
    #       put_in(env.extra[:words], fn(env, word, tokens) ->
    #         case fun.(env, word) do
    #           %Env{} = env -> {env, tokens}
    #           {%Env{} , _tokens} = ret -> ret
    #           _ -> built_ins(env, word, tokens)
    #         end
    #       end)
    #     _ -> put_in(env.extra[:words], &built_ins/3)
    #   end
    env =
      case opts[:externals] do
        fun when is_function(fun, 3) -> put_in(env.extra[:externals], fun)
        _ -> env
      end
    env = put_in(env.extra[:words], if(is_map(opts[:words]), do: opts[:words], else: built_in_words()))
    env = put_in(env.extra[:empty_stack_value], opts[:empty_stack_value] || :no_value)
    # env = put_in(env.extra[:always_uppercase_words], if(opts[:case_insensitive], do: true, else: false))
    env = interpret_tokens(env, commands)
    case env.stack do
      [result] -> {:ok, {env, result}}
      results -> {:ok, {env, results}}
    end
  end



  ### Helpers

  defmacro is_prim(binding), do: quote(do:
    is_integer(unquote(binding)) or
    is_float(unquote(binding)) or
    is_atom(unquote(binding)) or
    is_binary(unquote(binding))
  )



  ### Interpretation

  def reduction(env, word, tokens) do
    case env.extra[:reduction_handler] do
      nil -> {env, word, tokens}
      fun -> fun.(env, word, tokens)
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
        %Env{} = env -> stack_push(env, {:no_external, name, args})
        result -> stack_push(env, result)
      end
    else
      {env, args} -> stack_push(env, {:not_enough_args_on_stack_for, name, arity, args})
      _nofunction -> stack_push(env, {:no_externals, name, arity})
    end
  end


  def word_call(env, word, tokens) do
    {env, word, tokens} = reduction(env, word, tokens)
    case env.extra.words[word] do
      word_tokens when is_list(word_tokens) ->
        {env, word_tokens ++ tokens}
      fun when is_function(fun, 3) ->
        case fun.(env, word, tokens) do
          {%Env{}, tokens} = result when is_list(tokens) -> result
          %Env{} = env -> {env, tokens}
        end
      fun when is_function(fun, 1) ->
        case fun.(env) do
          %Env{} = env -> {env, tokens}
        end
      nil ->
        env = push_unknown_word_msg(env, word)
        {env, tokens}
    end
  end


  def stack_push(env, values) when is_list(values) do
    %{env |
      stack: :lists.reverse(values, env.stack) # values ++ env.stack,
    }
  end
  def stack_push(env, value) do
    %{env |
      stack: [value | env.stack],
    }
  end

  def stack_pop(env, count \\ 1) when count>=0 do
    {stack, values} = do_stack_split(env.extra.empty_stack_value, count, env.stack)
    {%{env | stack: stack}, values}
  end
  defp do_stack_split(default_value, count, stack, values \\ [])
  defp do_stack_split(_default_value, 0, stack, values),       do: {stack, values}
  defp do_stack_split(default_value, count, [], values),       do: do_stack_split(default_value, count-1, [], [default_value | values])
  defp do_stack_split(default_value, count, [v|rest], values), do: do_stack_split(default_value, count-1, rest, [v | values])


  def interpret_tokens(env, tokens)
  def interpret_tokens(env, []), do: env
  def interpret_tokens(env, [int | tokens]) when is_integer(int), do: interpret_tokens(stack_push(env, int), tokens)
  def interpret_tokens(env, [float | tokens]) when is_float(float), do: interpret_tokens(stack_push(env, float), tokens)
  def interpret_tokens(env, [atom | tokens]) when is_atom(atom), do: interpret_tokens(stack_push(env, atom), tokens)
  def interpret_tokens(env, [string | tokens]) when is_binary(string), do: interpret_tokens(stack_push(env, string), tokens)
  def interpret_tokens(env, [{:external, {name, arity}} | tokens]), do: interpret_tokens(external_call(env, name, arity), tokens)
  def interpret_tokens(env, [{:word, _word, word_tokens} | tokens]) when is_list(word_tokens) do
    interpret_tokens(env, word_tokens++tokens)
  end
  def interpret_tokens(env, [{:word, word, fun} | tokens]) when is_function(fun, 3) do
    case fun.(env, word, tokens) do
      {%Env{} = env, tokens} when is_list(tokens) -> interpret_tokens(env, tokens)
      %Env{} = env -> interpret_tokens(env, tokens)
    end
  end
  def interpret_tokens(env, [{:word, _word, fun} | tokens]) when is_function(fun, 1) do
    case fun.(env) do
      %Env{} = env -> interpret_tokens(env, tokens)
    end
  end
  def interpret_tokens(env, [{:word, word, nil} | tokens]) do
    {env, tokens} = word_call(env, word, tokens)
    interpret_tokens(env, tokens)
  end
  def interpret_tokens(_env, tokens), do: throw {:ExForth, :invalid_tokens, tokens}


  def verify_tokens(tokens)
  def verify_tokens([]), do: :ok
  def verify_tokens([prim | tokens]) when is_prim(prim), do: verify_tokens(tokens)
  def verify_tokens([{:external, {_name, arity}} | tokens]) when is_integer(arity), do: verify_tokens(tokens)
  def verify_tokens([{:word, word, word_tokens} | tokens]) when is_binary(word) and is_list(word_tokens) do
    case verify_tokens(word_tokens) do
      {:error, _error_token}=error -> error
      :ok -> verify_tokens(tokens)
    end
  end
  def verify_tokens([{:word, word, nil} | tokens]) when is_binary(word), do: verify_tokens(tokens)
  def verify_tokens([{:word, word, fun} | tokens]) when is_binary(word) and is_function(fun, 3), do: verify_tokens(tokens)
  def verify_tokens([{:word, word, fun} | tokens]) when is_binary(word) and is_function(fun, 1), do: verify_tokens(tokens)
  def verify_tokens([error_token | _tokens]), do: {:error, error_token}



  # defp get_called_words_from_tokens(tokens)
  # defp get_called_words_from_tokens([]), do: []
  # defp get_called_words_from_tokens([{:word, word} | tokens]), do: [word | get_called_words_from_tokens(tokens)]
  # defp get_called_words_from_tokens([_ | tokens]), do: get_called_words_from_tokens(tokens)


  defp decorate_words_with_tokens(tokens, word_map) do
    Enum.map(tokens, fn
      {:word, word, nil} -> {:word, word, word_map[word]}
      token -> token
    end)
  end


  def unknown_word_msg(word)           ,do: {:unknown_word, word}
  def push_unknown_word_msg(env, word) ,do: stack_push(env, unknown_word_msg(word))

  def invalid_arguments_msg(word, args) when is_list(args), do: {:error, :invalid_arguments, word, args}
  def push_invalid_arguments_msg(env, word, args)         , do: stack_push(env, invalid_arguments_msg(word, args))


  defp parse_function_tokens(env, token)
  defp parse_function_tokens(env, [{:word, ";", _} | tokens]) do
    {env, [], tokens}
  end
  defp parse_function_tokens(env, [{:word, "`UNQUOTE", _} | tokens]) do
    {env, unquoted_tokens} =
      case stack_pop(env, 1) do
        {env, [unquoted_tokens]} when is_list(unquoted_tokens) -> {env, unquoted_tokens}
        {env, [unquoted_token]} -> {env, [unquoted_token]}
      end
    {env, word_tokens, tokens} = parse_function_tokens(env, tokens)
    case verify_tokens(unquoted_tokens) do
      {:error, error_token} ->
        msg = "( INVALID UNQUOTE OF TOKEN `#{inspect error_token}` IN: #{inspect word_tokens} )"
        {env, [msg | word_tokens], tokens}
      :ok ->
        {env, unquoted_tokens++word_tokens, tokens}
    end
  end
  defp parse_function_tokens(env, [token | tokens]) do
    {env, word_tokens, tokens} = parse_function_tokens(env, tokens)
    {env, [token | word_tokens], tokens}
  end


  def built_in_words(), do: %{

    ## Comments
    "(" => fn(env, _word, tokens) ->
      [_ | tokens] = Enum.drop_while(tokens, fn {:word, ")", _} -> false; _ -> true end)
      {env, tokens}
    end,

    ## Function definition
    ":" => fn(env, _word, tokens) ->
      [{:word, name, _} | tokens] = tokens
      # {word_tokens, [_semicolon | tokens]} = Enum.split_while(tokens, fn {:word, ";", _} -> false; _ -> true end)
      {env, word_tokens, tokens} = parse_function_tokens(env, tokens)
      word_tokens = decorate_words_with_tokens(word_tokens, env.extra.words)
      env = put_in(env.extra.words[name], word_tokens)
      {env, tokens}
    end,

    ## Quoting, Unquoting, and helpers

    "QUOTE" => fn
      (env, _word, [token | tokens]) ->
        [token] = decorate_words_with_tokens([token], env.extra.words)
        env = stack_push(env, token)
        {env, tokens}
      (env, word, tokens) ->
        env = push_invalid_arguments_msg(env, word, [])
        {env, tokens}
    end,

    "UNQUOTE" => fn(env, word, tokens) ->
      {env, unquoted_tokens} =
        case stack_pop(env, 1) do
          {env, [unquoted_tokens]} when is_list(unquoted_tokens) -> {env, unquoted_tokens}
          {env, [unquoted_token]} -> {env, [unquoted_token]}
        end
      case verify_tokens(unquoted_tokens) do
        {:error, error_token} ->
          msg = "( INVALID UNQUOTE IN WORD `#{word}` OF TOKEN `#{inspect error_token}` IN: #{inspect unquoted_tokens} )"
          {env, [msg | tokens]}
        :ok ->
          {env, unquoted_tokens++tokens}
      end
    end,

    "WORD" => fn(env, word, tokens) ->
      case stack_pop(env, 1) do
        {env, [name]} when is_binary(name) ->
          [word_token] = decorate_words_with_tokens([{:word, name, nil}], env.extra.words)
          env = stack_push(env, word_token)
          {env, tokens}
        {env, [invalid]} ->
          env = push_invalid_arguments_msg(env, word, [invalid])
          {env, tokens}
      end
    end,

    ## Function forgetting
    ";" => fn
      (env, _word, [{:word, word, _word_tokens} | tokens]) ->
        {_maybe_word_def, env} = pop_in(env.extra.words[word])
        {env, tokens}
      (env, word, [token | tokens]) ->
        env = push_invalid_arguments_msg(env, word, [token])
        {env, tokens}
      (env, word, tokens) ->
        env = push_invalid_arguments_msg(env, word, [])
        {env, tokens}
    end,

    ## Numbers
    "+" => fn(env, word, tokens) ->
      case stack_pop(env, 2) do
        {env, [t1, t0]} when (is_integer(t0) or is_float(t0)) and (is_integer(t1) or is_float(t1)) ->
          env = stack_push(env, t1+t0)
          {env, tokens}
        {env, [t1, t0]} ->
          env = push_invalid_arguments_msg(env, word, [t1, t0])
          {env, tokens}
      end
    end,

    "-" => fn(env, word, tokens) ->
      case stack_pop(env, 2) do
        {env, [t1, t0]} when (is_integer(t0) or is_float(t0)) and (is_integer(t1) or is_float(t1)) ->
          env = stack_push(env, t1-t0)
          {env, tokens}
        {env, [t1, t0]} ->
          env = push_invalid_arguments_msg(env, word, [t1, t0])
          {env, tokens}
      end
    end,

    "*" => fn(env, word, tokens) ->
      case stack_pop(env, 2) do
        {env, [t1, t0]} when (is_integer(t0) or is_float(t0)) and (is_integer(t1) or is_float(t1)) ->
          env = stack_push(env, t1*t0)
          {env, tokens}
        {env, [t1, t0]} ->
          env = push_invalid_arguments_msg(env, word, [t1, t0])
          {env, tokens}
      end
    end,

    "/" => fn(env, word, tokens) ->
      case stack_pop(env, 2) do
        {env, [t1, t0]} when is_integer(t0) and is_integer(t1) ->
          env = stack_push(env, div(t1, t0))
          {env, tokens}
        {env, [t1, t0]} when (is_integer(t0) or is_float(t0)) and (is_integer(t1) or is_float(t1)) ->
          env = stack_push(env, t1/t0)
          {env, tokens}
        {env, [t1, t0]} ->
          env = push_invalid_arguments_msg(env, word, [t1, t0])
          {env, tokens}
      end
    end,

    ## List
    "[]" => fn(env, _word, tokens) ->
      env = stack_push(env, [[]])
      {env, tokens}
    end,

    "[]N" => fn(env, word, tokens) ->
      case stack_pop(env, 1) do
        {env, [arity]} when is_integer(arity) and arity >=0 ->
          {env, lst} = stack_pop(env, arity)
          env = stack_push(env, [lst])
          {env, tokens}
        {env, [arity]} ->
          env = push_invalid_arguments_msg(env, word, [arity])
          {env, tokens}
      end
    end,

    "::" => fn(env, word, tokens) ->
      case stack_pop(env, 2) do
        {env, [value, lst]} when is_list(lst) ->
          lst = [value | lst]
          env = stack_push(env, [lst])
          {env, tokens}
        {env, [value, lst]} ->
          env = push_invalid_arguments_msg(env, word, [value, lst])
          {env, tokens}
      end
    end,

    "::N" => fn(env, word, tokens) ->
      case stack_pop(env, 2) do
        {env, [lst, arity]} when is_integer(arity) and arity >=0 and is_list(lst) ->
          {env, args} = stack_pop(env, arity)
          lst = List.foldr(args, lst, fn(value, lst) -> [value | lst] end)
          env = stack_push(env, [lst])
          {env, tokens}
        {env, [lst, arity]} ->
          env = push_invalid_arguments_msg(env, word, [lst, arity])
          {env, tokens}
      end
    end,

    # "MAP" => fn(env, word, tokens) ->
    #   case stack_pop(env, 2) do
    #     {env, [lst, unquoted_tokens]} when is_list(lst) ->
    #       case verify_tokens(List.wrap(unquoted_tokens)) do
    #         {:error, error_token} ->
    #           msg = "( INVALID UNQUOTE IN WORD `#{word}` OF TOKEN `#{inspect error_token}` IN: #{inspect unquoted_tokens} )"
    #           {env, [msg | tokens]}
    #         :ok ->
    #
    #           Enum.reduce(lst, env, fn(value, env) ->
    #             env = stack_push(env, [value])
    #             unquoted_tokens
    #           end)
    #           {env, tokens}
    #       end
    #   end
    # end,

    ## Call externals through the FFI directory
    "FFI" => fn(env, word, tokens) ->
      case stack_pop(env, 3) do
        {env, [arity, name, modules]} when is_integer(arity) and arity>=0 and is_list(modules) ->
          name =
            case modules do
              [] -> name
              modules -> {modules, name}
            end
          {env, args} = stack_pop(env, arity)
          env = external_call(env, name, args)
        {env, [arity, name, modules]} ->
          env = push_invalid_arguments_msg(env, word, [arity, name, modules])
          {env, tokens}
      end
    end,

    ## Basic forth calls
    "DUP" => fn(env, _word, tokens) ->
      {env, [a]} = stack_pop(env)
      env = stack_push(env, [a, a])
      {env, tokens}
    end,

    "DROP" => fn(env, _word, tokens) ->
      {env, _} = stack_pop(env, 1)
      {env, tokens}
    end,

    "SWAP" => fn(env, _word, tokens) ->
      {env, [t1, t0]} = stack_pop(env, 2)
      env = stack_push(env, [t0, t1])
      {env, tokens}
    end,

    "OVER" => fn(env, _word, tokens) ->
      {env, [t1, t0]} = stack_pop(env, 2)
      env = stack_push(env, [t1, t0, t1])
      {env, tokens}
    end,

    "ROT" => fn(env, _word, tokens) ->
      {env, [t2, t1, t0]} = stack_pop(env, 3)
      env = stack_push(env, [t1, t0, t2])
      {env, tokens}
    end,

    "SEE" => fn(env, word, tokens) ->
      {opts, token, tokens} =
        case tokens do
          [:formatted, token | tokens] ->
            {[tokens_per_line: :auto], token, tokens}
          [token | tokens] -> {[], token, tokens}
        end
      output =
        case token do
          {:word, word, nil} ->
            case env.extra.words[word] do
              word_tokens when is_list(word_tokens) -> stringize(word_tokens, opts)
              nil -> "( WORD `#{word}` IS NOT DEFINED )"
              _ -> "( NATIVE WORD `#{word}` )"
            end
          {:word, _word, word_tokens} when is_list(word_tokens) -> stringize(word_tokens, opts)
          {:word, _word, fun} when is_function(fun) -> "( NATIVE WORD `#{word}` )"
          token -> invalid_arguments_msg(word, [token])
        end
      env = stack_push(env, output)
      {env, tokens}
    end,

    # ":" => fn(env, word, tokens) ->
    # end,

  }



  ### Tokanization

  def stringize(tokens, opts \\ %{}) do
    case opts[:tokens_per_line] do
      nil ->
        string_list = stringize_tokens(tokens)
        Enum.join(string_list, " ")
      :auto ->
        format_stringize_tokens(opts[:formatting_options] || :full, tokens)
      tokens_per_line when is_integer(tokens_per_line) ->
        tokens
        |> stringize_tokens()
        |> Enum.chunk(tokens_per_line, tokens_per_line, [])
        |> Enum.map(&Enum.join(&1, " "))
        |> Enum.join("\n")
    end
  end

  def format_stringize_tokens(tokens, opts \\ %{}) do
    opts =
      if opts === :full do
        %{
          newline_around_word_definitions: true,
          newline_at_prim_to_notprim: true,
        }
      else
        opts
      end

    formatted_tokens = format_tokens(opts, tokens)
    stringize_tokens(formatted_tokens)
  end

  def format_tokens(opts, tokens)
  def format_tokens(_opts, []), do: []
  def format_tokens(%{newline_around_word_definitions: true}=opts, [{:format, ?\n}=formatted, {:word, ":", _}=token | tokens]) do
    [formatted, token | format_tokens(put_in(opts[:word_def], true), tokens)]
  end
  def format_tokens(%{newline_around_word_definitions: true}=opts, [{:word, ":", _}=token | tokens]) do
    [{:format, ?\n}, token | format_tokens(put_in(opts[:word_def], true), tokens)]
  end
  def format_tokens(%{newline_around_word_definitions: true, in_def: true}=opts, [{:word, ";", _}=token | tokens]) do
    [token, {:format, ?\n} | format_tokens(put_in(opts[:word_def], false), tokens)]
  end
  def format_tokens(%{newline_at_prim_to_notprim: true}=opts, [prim, notprim | tokens]) when is_prim(prim) and not(is_prim(notprim)) do
    [prim, {:format, ?\n}, notprim | format_stringize_tokens(opts, tokens)]
  end
  def format_tokens(opts, [token | tokens]), do: [{:format, ?\s}, token | format_tokens(opts, tokens)]

  def stringize_tokens(opts \\ %{}, tokens)
  def stringize_tokens(_opts, []), do: []
  def stringize_tokens(opts, [{:format, str} | tokens]), do: [str | stringize_tokens(opts, tokens)]
  def stringize_tokens(opts, [{:word, word, _} | tokens]), do: [word | stringize_tokens(opts, tokens)]
  def stringize_tokens(opts, [{:external, {name, arity}} | tokens]) when is_atom(name) do
    ["#{name}/#{arity}" | stringize_tokens(opts, tokens)]
  end
  def stringize_tokens(opts, [{:external, {{modules, name}, arity}} | tokens]) do
    modules = "#{Enum.join(modules, ".")}"
    ["#{modules}.#{name}/#{arity}" | stringize_tokens(opts, tokens)]
  end
  def stringize_tokens(opts, [int | tokens]) when is_integer(int)      ,do: [to_string(int)   | stringize_tokens(opts, tokens)]
  def stringize_tokens(opts, [atom | tokens]) when is_atom(atom)       ,do: [to_string(atom)  | stringize_tokens(opts, tokens)]
  def stringize_tokens(opts, [float | tokens]) when is_float(float)    ,do: [to_string(float) | stringize_tokens(opts, tokens)]
  def stringize_tokens(opts, [string | tokens]) when is_binary(string) ,do: [string           | stringize_tokens(opts, tokens)]


  defp tokanize(opts, input) do
    result = parse_forth(input)

    result =
      if opts[:always_uppercase_words] do
        Enum.map(result, fn
          {:word, word, data} when is_binary(word) -> {:word, String.upcase(word), data}
          token -> token
        end)
      else
        result
      end

    {:ok, result}
  end


  defp parse_forth(input) do
    case parse(input, forth_parser()) do
    # case parse(input, forth_parser()) do
      %{error: nil, result: result, rest: ""} -> result
      %{error: nil, result: result, rest: rest} -> throw {:ExForth, :input_not_consumed, result, rest}
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
      {float, rest} ->
        %{context |
          result: float,
          rest: rest,
        }
      _ ->
        %{context |
          error: %ExSpirit.Parser.ParseException{message: "Parsing a floating point number failed", context: context},
        }
    end
  end

  defrule nonquote_char_or_escaped_quote(alt([
    lit(?\\) |> alt([
      lit(?n) |> success(?\n),
      lit(?r) |> success(?\r),
      lit(?t) |> success(?\t),
      lit(?s) |> success(?\s),
      char(),
    ]),
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
    chars1([?A..?Z, ?a..?z, ?_], [?A..?Z, ?a..?z, ?_, ?0..?9, ??, ?!]),
  ])), pipe_result_into: :erlang.iolist_to_binary()

  defrule forth_parser_to_existing_atom(context, opts \\ []) do
    try do
      atom = String.to_existing_atom(context.result)
      %{context | result: atom}
    rescue
      ArgumentError ->
        case opts[:else] do
          :return -> context
          _ ->
            %{context |
              error: %ExSpirit.Parser.ParseException{message: "Unable to convert the binary `#{context.result} to an existing atom`", context: context, extradata: context.result},
              result: nil,
            }
        end
    end
  end

  defrule forth_parser_atom(lit(?:) |> alt([
    forth_parser_atom_string() |> forth_parser_to_existing_atom(),
    symbols(unquote(Macro.escape(elixir_operators))),
  ]))

  defrule forth_parser_string(lit(?") |> no_skip(seq([
    repeat(nonquote_char_or_escaped_quote(), 1),
    lit(?"),
  ]))), pipe_result_into: :erlang.iolist_to_binary()

  defrule forth_parser_external_call(seq([
    alt([
      seq([
        alt([forth_parser_atom(), symbols(unquote(Macro.escape(elixir_operators))), forth_parser_atom_string() |> forth_parser_to_existing_atom(else: :return)]),
        repeat(seq([
          lit(?.), alt([symbols(unquote(Macro.escape(elixir_operators))), forth_parser_atom_string() |> forth_parser_to_existing_atom(else: :return)])
        ]))
      ]),
    ]),
    lit(?/),
    uint()
    ])), pipe_result_into: (case do
      [[name], arity] -> {name, arity}
      [[_|_]=names, arity] ->
        {name, modules} = List.pop_at(names, -1)
        {{modules, name}, arity}
      [name, arity] -> {name, arity}
    end)

  def make_word_tuple(word, data \\ nil), do: {:word, word, data}

  defrule forth_parser_token(alt([
    lit(?-) |> forth_parser_integer()            |> ignore(forth_parser_token_separator(), pass_result: true) |> pipe_result_into(Kernel.-()),
    forth_parser_integer()                       |> ignore(forth_parser_token_separator(), pass_result: true),
    forth_parser_float()                         |> ignore(forth_parser_token_separator(), pass_result: true),
    forth_parser_atom()                          |> ignore(forth_parser_token_separator(), pass_result: true),
    tag(:external, forth_parser_external_call()) |> ignore(forth_parser_token_separator(), pass_result: true),
    forth_parser_string()                        |> ignore(forth_parser_token_separator(), pass_result: true),
    forth_parser_token_not_separator()           |> ignore(forth_parser_token_separator(), pass_result: true) |> pipe_result_into(make_word_tuple()),
  ]))

  token_separators = [?\s,  ?\n,  ?\r,  ?\t]

  defrule forth_parser_token_separator(alt([chars(unquote(token_separators)), eoi()]))
  defrule forth_parser_token_not_separator(chars(unquote(Enum.map(token_separators, &Kernel.-/1))))

  defrule forth_parser(alt([forth_parser_token_separator(), success()]) |> repeat(forth_parser_token()) |> eoi(pass_result: true))

end
