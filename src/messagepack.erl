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

-module(messagepack).

-include("internal.hrl").

%% -- public --
-export([encode/1, encode/2,
         decode/1, decode/2]).

%% == public ==

-spec encode(term()) -> {ok, <<_:8,_:_*8>>} | {error, {badarg, _}}.
encode(Term) ->
    encode(Term, []).

-spec encode(term(), keyword()) -> {ok, <<_:8,_:_*8>>} | {error, {badarg, _}}.
encode(Term, Options) ->
    messagepack_encodable:encode(Term, Options).

-spec decode(term()) -> {ok, term()} | {error, {badarg, _}}.
decode(Term) ->
    decode(Term, []).

-spec decode(term(), keyword()) -> {ok, term()} | {error, {badarg, _}}.
decode(Term, Options) ->
    messagepack_decodable:decode(Term, Options).
