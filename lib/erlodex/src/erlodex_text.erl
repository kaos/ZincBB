-module(erlodex_text).

-export([process/1, process_block/1, tokenize/1]).

-define(TOKENIZER, "\\W+").


process_block(Block) -> 
    Tokens = tokenize(Block), 
    [process(T) || T <- Tokens].

process(Token) -> 
    casefold(Token).

casefold(Token) ->
    Mixed = binary_to_list(Token),
    Lower = string:to_lower(Mixed),
    list_to_binary(Lower).

tokenize(Block) -> 
    re:split(Block, ?TOKENIZER, [{return, binary}, trim]).
