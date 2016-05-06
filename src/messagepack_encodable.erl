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

-module(messagepack_encodable).

-include("internal.hrl").

%% -- public --
-export([encode/2]).

%% -- private --
-export([encode_atom/2, encode_bitstring/2, encode_float/2, encode_function/2,
         encode_integer/2, encode_list/2, encode_map/2, encode_pid/2,
         encode_port/2, encode_reference/2, encode_string/2, encode_tuple/2]).
-export([encode_any/2]).

%% == public ==

-spec encode(term(), keyword()) -> {ok, <<_:8,_:_*8>>} | {error, {badarg, _}}.
encode(Term, Options) when is_atom(Term) ->
    encode_atom(Term, Options);
encode(Term, Options) when is_integer(Term) ->
    encode_integer(Term, Options);
encode(Term, Options) when is_float(Term) ->
    encode_float(Term, Options);
encode(Term, Options) when is_bitstring(Term) ->
    encode_bitstring(Term, Options);
encode(Term, Options) when is_tuple(Term) ->
    encode_tuple(Term, Options);
encode(Term, Options) when is_list(Term) ->
    encode_list(Term, Options);
encode(Term, Options) when is_map(Term) ->
    encode_map(Term, Options);
encode(Term, Options) when is_function(Term) ->
    encode_function(Term, Options);
encode(Term, Options) when is_pid(Term) ->
    encode_pid(Term, Options);
encode(Term, Options) when is_port(Term) ->
    encode_port(Term, Options);
encode(Term, Options) when is_reference(Term) ->
    encode_reference(Term, Options).

%% == private ==

encode_atom(nil, _O) ->
    {ok, <<16#c0:8>>};
encode_atom(false, _O) ->
    {ok, <<16#c2:8>>};
encode_atom(true, _O) ->
    {ok, <<16#c3:8>>};
encode_atom(A, O) ->
    encode_string(atom_to_binary(A, utf8), O).

encode_bitstring(B, O) when 0 =:= bit_size(B) rem 8 ->
    U = byte_size(B),
    if ?U8_MAX  >= U -> {ok, <<16#c4, U: 8/big-unsigned, B/binary>>};
       ?U16_MAX >= U -> {ok, <<16#c5, U:16/big-unsigned, B/binary>>};
       ?U32_MAX >= U -> {ok, <<16#c6, U:32/big-unsigned, B/binary>>};
       true          -> error_badarg(B, O)
    end;
encode_bitstring(B, O) ->
    error_badarg(B, O).

encode_float(F, _O) ->
    {ok, <<16#cb, F:64/big-float>>}. % 64bit ONLY

encode_function(F, O) ->
    encode_ext(?EXT_FUNCTION, term_to_binary(F), O).

encode_integer(U, O) when U >= 0 ->
    if ?U7_MAX  >= U -> {ok, << 2#0:1,  U: 7/big-unsigned>>};
       ?U8_MAX  >= U -> {ok, <<16#cc:8, U: 8/big-unsigned>>};
       ?U16_MAX >= U -> {ok, <<16#cd:8, U:16/big-unsigned>>};
       ?U32_MAX >= U -> {ok, <<16#ce:8, U:32/big-unsigned>>};
       ?U64_MAX >= U -> {ok, <<16#cf:8, U:64/big-unsigned>>};
       true          -> error_badarg(U, O)
    end;
encode_integer(I, O) ->
    if ?I6_MIN  =< I -> {ok, << 2#111:3, I: 5/big-signed>>};
       ?I8_MIN  =< I -> {ok, <<16#d0:8,  I: 8/big-signed>>};
       ?I16_MIN =< I -> {ok, <<16#d1:8,  I:16/big-signed>>};
       ?I32_MIN =< I -> {ok, <<16#d2:8,  I:32/big-signed>>};
       ?I64_MIN =< I -> {ok, <<16#d3:8,  I:64/big-signed>>};
       true          -> error_badarg(I, O)
    end.

encode_list(L, O) when ?U32_MAX >= length(L) ->
    encode_list(L, O, []);
encode_list(L, O) ->
    error_badarg(L, O).

encode_map(M, O) when ?U32_MAX >= map_size(M) ->
    encode_map(maps:to_list(M), O, []);
encode_map(M, O) ->
    error_badarg(M, O).

encode_pid(P, O) ->
    encode_ext(?EXT_PID, term_to_binary(P), O).

encode_port(P, O) ->
    encode_ext(?EXT_PORT, term_to_binary(P), O).

encode_reference(R, O) ->
    encode_ext(?EXT_REFERENCE, term_to_binary(R), O).

encode_string(S, O) ->
    U = byte_size(S),
    if ?U5_MAX  >= U -> {ok, << 2#101:3, U: 5/big-unsigned, S/binary>>};
       ?U8_MAX  >= U -> {ok, <<16#d9,    U: 8/big-unsigned, S/binary>>}; % limit(atom)
    %  ?U16_MAX >= U -> {ok, <<16#da,    U:16/big-unsigned, S/binary>>}
    %  ?U32_MAX >= U -> {ok, <<16#db,    U:32/big-unsigned, S/binary>>}
       true          -> error_badarg(S, O)
    end.

encode_tuple(T, O) when ?U32_MAX >= tuple_size(T) -> % ?U26_MAX, limit(tuple)
    encode_tuple(tuple_to_list(T), O, []);
encode_tuple(T, O) ->
    error_badarg(T, O).


encode_any(T, O) when is_map(T) ->
    case maps:is_key('__struct__', T) of % defstruct
        true ->
            encode_map(T, O);
        false ->
            error_badarg(T, O)
    end.

%% == internal ==

encode_ext(E, B, O) ->
    U = byte_size(B),
    if 1       =:= U -> {ok, <<16#d4, E, B/binary>>};
       2       =:= U -> {ok, <<16#d5, E, B/binary>>};
       4       =:= U -> {ok, <<16#d6, E, B/binary>>};
       8       =:= U -> {ok, <<16#d7, E, B/binary>>};
       16      =:= U -> {ok, <<16#d8, E, B/binary>>};
       ?U8_MAX  >= U -> {ok, <<16#c7, U: 8/big-unsigned, E, B/binary>>};
       ?U16_MAX >= U -> {ok, <<16#c8, U:16/big-unsigned, E, B/binary>>};
       ?U32_MAX >= U -> {ok, <<16#c9, U:32/big-unsigned, E, B/binary>>};
       true          -> error_badarg(B, O)
    end.

encode_list([], _O, L) ->
    U = length(L),
    B = list_to_binary(lists:reverse(L)),
    if ?U4_MAX  >= U -> {ok, << 2#1001:4, U: 4/big-unsigned, B/binary>>};
       ?U16_MAX >= U -> {ok, <<16#dc,     U:16/big-unsigned, B/binary>>};
       true          -> {ok, <<16#dd,     U:32/big-unsigned, B/binary>>}
    end;
encode_list([H|T], O, L) ->
    case encode(H, O) of
        {ok, B} ->
            encode_list(T, O, [B|L]);
        {error, Reason} ->
            {error, Reason}
    end.

encode_map([], _O, L) ->
    U = length(L),
    B = list_to_binary(lists:reverse(L)),
    if ?U4_MAX  >= U -> {ok, << 2#1000:4, U: 4/big-unsigned, B/binary>>};
       ?U16_MAX >= U -> {ok, <<16#de,     U:16/big-unsigned, B/binary>>};
       true          -> {ok, <<16#df,     U:32/big-unsigned, B/binary>>}
    end;
encode_map([{K,V}|T], O, L) ->
    case encode(K, O) of
        {ok, KB} ->
            case encode(V, O) of
                {ok, VB} ->
                    encode_map(T, O, [<<KB/binary, VB/binary>>|L]);
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

encode_tuple([], O, L) ->
    encode_ext(?EXT_TUPLE, list_to_binary(lists:reverse(L)), O);
encode_tuple([H|T], O, L) ->
    case encode(H, O) of
        {ok, B} ->
            encode_tuple(T, O, [B|L]);
        {error, Reason} ->
            {error, Reason}
    end.

error_badarg(T, _O) ->
    {error, {badarg, T}}.
