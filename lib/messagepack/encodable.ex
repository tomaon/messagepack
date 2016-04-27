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

defprotocol Messagepack.Encodable do
  @fallback_to_any true
  def encode(term, options)
end

defimpl Messagepack.Encodable, for: Atom do
  def encode(term, options), do: :messagepack_encodable.encode_atom(term, options)
end

defimpl Messagepack.Encodable, for: Integer do
  def encode(term, options), do: :messagepack_encodable.encode_integer(term, options)
end

defimpl Messagepack.Encodable, for: Float do
  def encode(term, options), do: :messagepack_encodable.encode_float(term, options)
end

defimpl Messagepack.Encodable, for: BitString do
  def encode(term, options), do: :messagepack_encodable.encode_bitstring(term, options)
end

defimpl Messagepack.Encodable, for: Tuple do
  def encode(term, options), do: :messagepack_encodable.encode_tuple(term, options)
end

defimpl Messagepack.Encodable, for: List do
  def encode(term, options), do: :messagepack_encodable.encode_list(term, options)
end

defimpl Messagepack.Encodable, for: Map do
  def encode(term, options), do: :messagepack_encodable.encode_map(term, options)
end

defimpl Messagepack.Encodable, for: Function do
  def encode(term, options), do: :messagepack_encodable.encode_function(term, options)
end

defimpl Messagepack.Encodable, for: PID do
  def encode(term, options), do: :messagepack_encodable.encode_pid(term, options)
end

defimpl Messagepack.Encodable, for: Port do
  def encode(term, options), do: :messagepack_encodable.encode_port(term, options)
end

defimpl Messagepack.Encodable, for: Reference do
  def encode(term, options), do: :messagepack_encodable.encode_reference(term, options)
end

defimpl Messagepack.Encodable, for: Any do
  def encode(term, options), do: :messagepack_encodable.encode_any(term, options)
end
