# [MessagePack][1] for [Elixir][2] / [Erlang][3]

[![Build Status](https://travis-ci.org/tomaon/messagepack.svg?branch=master)](https://travis-ci.org/tomaon/messagepack)

## Installation

Add `messagepack` to your list of dependencies in `mix.exs`:

```elixir
def deps do
  [{:messagepack, "x.y.z"}]
end
```

or `rebar.config`:
```erlang
{deps, [
        {messagepack, "x.y.z"},
       ]}.
```
## Example

elixir:
```elixir
iex(1)> defmodule Example do
...(1)>   defstruct [:compact, :schema]
...(1)> end
{:module, Example, <<...>>, %Example{compact: nil, schema: nil}}
iex(2)> x = %Example{:compact => true, :schema => 0}
%Example{compact: true, schema: 0}
iex(3)> x = Messagepack.decode!(Messagepack.encode!(x))
%Example{compact: true, schema: 0}
```

erlang:
```erlang
1> X = #{compact => true, schema => 0}.
#{compact => true,schema => 0}
2> {ok, B} = messagepack:encode(X).
{ok,<<130,167,99,111,109,112,97,99,116,195,166,115,99,104,101,109,97,0>>}
3> messagepack:decode(B).
{ok,#{compact => true,schema => 0}}
```

## License

Apache-2.0

[1]: http://msgpack.org/
[2]: http://elixir-lang.org/
[3]: https://www.erlang.org/
