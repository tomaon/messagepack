##
## Copyright (c) 2016 Tomohiko AONO
##
## Licensed under the Apache License, Version 2.0 (the "License");
## you may not use this file except in compliance with the License.
## You may obtain a copy of the License at
##
##    http://www.apache.org/licenses/LICENSE-2.0
##
## Unless required by applicable law or agreed to in writing, software
## distributed under the License is distributed on an "AS IS" BASIS,
## WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
## See the License for the specific language governing permissions and
## limitations under the License.
##

defmodule Messagepack do
  @moduledoc """
  MessagePack for Elixir / Erlang

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
  """

  alias Messagepack.Decodable
  alias Messagepack.Encodable
  alias Messagepack.Error

  @doc """
  Encode a term to Messagepack as binary.
  """
  @spec encode(term, Keyword.t) :: {:ok, binary} | {:error, {:badarg, term}}
  def encode(term, options \\ []), do: Encodable.encode(term, options)

  @doc """
  Encode a term to Messagepack as binary, raises an exception on error.
  """
  @spec encode!(term, Keyword.t) :: binary | no_return
  def encode!(term, options \\ []) do
    case encode(term, options) do
      {:ok, binary} ->
        binary
      {:error, {:badarg, value}} ->
        raise Error, value: value
    end
  end

  @doc """
  Decode Messagepack to a term.
  """
  @spec decode(binary, Keyword.t) :: {:ok, term} | {:error, {:badarg, binary}}
  def decode(binary, options \\ []), do: Decodable.decode(binary, options)

  @doc """
  Decode Messagepack to a term, raises an exception on error.
  """
  @spec decode!(binary, Keyword.t) :: term | no_return
  def decode!(binary, options \\ []) do
    case decode(binary, options) do
      {:ok, term} ->
        term
      {:error, {:badarg, value}} ->
        raise Error, value: value
    end
  end

end
