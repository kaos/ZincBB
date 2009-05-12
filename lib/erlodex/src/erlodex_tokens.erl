-module(erlodex_tokens).

-compile(export_all).

-define(TOKENIZER, "\\W+").

form(Input) ->
    Tokens = re:split(Input, ?TOKENIZER, [{return, binary}, trim]), length(Tokens).

process(Token) -> casefold(Token).

casefold(Token) ->
    Mixed = binary_to_list(Token),
    Lower = string:to_lower(Mixed),
    list_to_binary(Lower).
