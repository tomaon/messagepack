%%
%% Copyright (c) 2016 Tomohiko AONO
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%    http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%

-module(messagepack_decodable).

-include("internal.hrl").

%% -- public --
-export([decode/2]).

%% -- private --
-export([decode_bitstring/2]).

%% == public ==

-spec decode(term(), keyword()) -> {ok, term()} | {error, {badarg, _}}.
decode(Term, Options) when is_bitstring(Term) ->
    decode_bitstring(Term, Options);
decode(Term, Options) ->
    error_badarg(Term, Options).

%% == private ==

decode_bitstring(B, O) when 0 =:= bit_size(B) rem 8 ->
    case decode_binary(B, O, 0) of
        {ok, T, U} when U =:= byte_size(B) ->
            {ok, T};
        {error, Reason} ->
            {error, Reason}
    end;
decode_bitstring(B, O) ->
    error_badarg(B, O).

%% == internal ==

decode_binary(B, O, S) ->
    case binary_part(B, S, 1) of
        %% nil format family
        <<16#c0>> ->
            {ok, nil, S+1};
        %% bool format family
        <<16#c2>> ->
            {ok, false, S+1};
        <<16#c3>> ->
            {ok, true, S+1};
        %% int format family (unsigned)
        X when X >= <<16#00>>, X =< <<16#7f>> ->
            <<U:8/big-unsigned>> = binary_part(B, S, 1),
            {ok, U, S+1};
        <<16#cc>> ->
            <<U:8/big-unsigned>> = binary_part(B, S+1, 1),
            {ok, U, S+1+1};
        <<16#cd>> ->
            <<U:16/big-unsigned>> = binary_part(B, S+1, 2),
            {ok, U, S+1+2};
        <<16#ce>> ->
            <<U:32/big-unsigned>> = binary_part(B, S+1, 4),
            {ok, U, S+1+4};
        <<16#cf>> ->
            <<U:64/big-unsigned>> = binary_part(B, S+1, 8),
            {ok, U, S+1+8};
        %% int format family (signed)
        X when X >= <<16#e0>>, X =< <<16#ff>> ->
            <<2#11:2, I:6/big-signed>> = binary_part(B, S, 1),
            {ok, I, S+1};
        <<16#d0>> ->
            <<I:8/big-signed>> = binary_part(B, S+1, 1),
            {ok, I, S+1+1};
        <<16#d1>> ->
            <<I:16/big-signed>> = binary_part(B, S+1, 2),
            {ok, I, S+1+2};
        <<16#d2>> ->
            <<I:32/big-signed>> = binary_part(B, S+1, 4),
            {ok, I, S+1+4};
        <<16#d3>> ->
            <<I:64/big-signed>> = binary_part(B, S+1, 8),
            {ok, I, S+1+8};
        %% float format family
        <<16#ca>> ->
            <<F:32/big-float>> = binary_part(B, S+1, 4),
            {ok, F, S+1+4};
        <<16#cb>> ->
            <<F:64/big-float>> = binary_part(B, S+1, 8),
            {ok, F, S+1+8};
        %% str format family
        X when X >= <<16#a0>>, X =< <<16#bf>> ->
            <<2#101:3, U:5/big-unsigned>> = binary_part(B, S, 1),
            decode_str(B, O, S+1, U);
        <<16#d9>> ->
            <<U:8/big-unsigned>> = binary_part(B, S+1, 1),
            decode_str(B, O, S+1+1, U);
        <<16#da>> ->
            <<U:16/big-unsigned>> = binary_part(B, S+1, 2),
            decode_str(B, O, S+1+2, U);
        <<16#db>> ->
            <<U:32/big-unsigned>> = binary_part(B, S+1, 4),
            decode_str(B, O, S+1+4, U);
        %% bin format family
        <<16#c4>> ->
            <<U:8/big-unsigned>> = binary_part(B, S+1, 1),
            {ok, binary_part(B, S+1+1, U), S+1+1+U};
        <<16#c5>> ->
            <<U:16/big-unsigned>> = binary_part(B, S+1, 2),
            {ok, binary_part(B, S+1+2, U), S+1+2+U};
        <<16#c6>> ->
            <<U:32/big-unsigned>> = binary_part(B, S+1, 4),
            {ok, binary_part(B, S+1+4, U), S+1+4+U};
        %% array format family
        X when X >= <<16#90>>, X =< <<16#9f>> ->
            <<2#1001:4, U:4/big-unsigned>> = binary_part(B, S, 1),
            decode_array(B, O, S+1, U, []);
        <<16#dc>> ->
            <<U:16/big-unsigned>> = binary_part(B, S+1, 2),
            decode_array(B, O, S+1+2, U, []);
        <<16#dd>> ->
            <<U:32/big-unsigned>> = binary_part(B, S+1, 4),
            decode_array(B, O, S+1+4, U, []);
        %% map format family
        X when X >= <<16#80>>, X =< <<16#8f>> ->
            <<2#1000:4, U:4/big-unsigned>> = binary_part(B, S, 1),
            decode_map(B, O, S+1, U, #{});
        <<16#de>> ->
            <<U:16/big-unsigned>> = binary_part(B, S+1, 2),
            decode_map(B, O, S+1+2, U, #{});
        <<16#df>> ->
            <<U:32/big-unsigned>> = binary_part(B, S+1, 4),
            decode_map(B, O, S+1+4, U, #{});
        %% ext format family
        <<16#c7>> ->
            <<U:8/big-unsigned, E>> = binary_part(B, S+1, 2),
            decode_ext(B, O, S+1+2, E, U);
        <<16#c8>> ->
            <<U:16/big-unsigned, E>> = binary_part(B, S+1, 3),
            decode_ext(B, O, S+1+3, E, U);
        <<16#c9>> ->
            <<U:32/big-unsigned, E>> = binary_part(B, S+1, 5),
            decode_ext(B, O, S+1+5, E, U);
        <<16#d4>> ->
            <<E>> = binary_part(B, S+1, 1),
            decode_ext(B, O, S+1+1, E, 1);
        <<16#d5>> ->
            <<E>> = binary_part(B, S+1, 1),
            decode_ext(B, O, S+1+1, E, 2);
        <<16#d6>> ->
            <<E>> = binary_part(B, S+1, 1),
            decode_ext(B, O, S+1+1, E, 4);
        <<16#d7>> ->
            <<E>> = binary_part(B, S+1, 1),
            decode_ext(B, O, S+1+1, E, 8);
        <<16#d8>> ->
            <<E>> = binary_part(B, S+1, 1),
            decode_ext(B, O, S+1+1, E, 16);
        X ->
            error_badarg(X, O)
    end.


decode_array(_B, _O, S, 0, L) ->
    {ok, lists:reverse(L), S};
decode_array(B, O, S, C, L) ->
    case decode_binary(B, O, S) of
        {ok, E, U} ->
            decode_array(B, O, U, C-1, [E|L]);
        {error, Reason} ->
            {error, Reason}
    end.

decode_ext(B, O, S, ?EXT_TUPLE, C) ->
    decode_tuple(B, O, S, C, []);
decode_ext(B, _O, S, _E, C) ->
    {ok, binary_to_term(binary_part(B, S, C)), S+C}.

decode_map(_B, _O, S, 0, M) ->
    {ok, M, S};
decode_map(B, O, S, C, M) ->
    case decode_binary(B, O, S) of
        {ok, K, KU} ->
            case decode_binary(B, O, KU) of
                {ok, V, VU} ->
                    decode_map(B, O, VU, C-1, maps:put(K, V, M));
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

decode_str(B, _O, S, C) when ?U8_MAX >= C ->
    P = binary_part(B, S, C),
    try binary_to_existing_atom(P, utf8) of
        A ->
            {ok, A, S+C}
    catch
        error:badarg ->
            {ok, P, S+C}
    end;
decode_str(B, _O, S, C) ->
    {ok, binary_part(B, S, C), S+C}. % != badarg, == SystemLimitError

decode_tuple(_B, O, S, 0, L) ->
    U = length(L),
    if ?U26_MAX >= U -> {ok, list_to_tuple(lists:reverse(L)), S};
       true          -> error(U, O)
    end;
decode_tuple(B, O, S, C, L) ->
    case decode_binary(B, O, S) of
        {ok, E, U} ->
            decode_tuple(B, O, U, C-(U-S), [E|L]);
        {error, Reason} ->
            {error, Reason}
    end.

error_badarg(T, _O) ->
    {error, {badarg, T}}.
