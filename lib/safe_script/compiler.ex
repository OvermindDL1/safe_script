defmodule SafeScript.Compiler do

  @type input :: binary()
  @type compiled_form :: any()
  @type interpreted_result :: {:ok, {SafeScript.Env.t, any()}} | {:error, any()}

  @callback compile_expressions(input) :: {:ok, compiled_form} | {:error, any()}
  @callback compile_expressions(input, list) :: {:ok, compiled_form} | {:error, any()}

  @callback interpret_forms(compiled_form) :: interpreted_result
  @callback interpret_forms(compiled_form, list) :: interpreted_result

end
