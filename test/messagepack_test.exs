defmodule MessagepackTest do
  use ExUnit.Case, async: true

  alias Messagepack

  # @see
  #  http://erlang.org/doc/efficiency_guide/advanced.html > 10.2 System Limits
  #  http://erlang.org/doc/apps/erts/erl_ext_dist.html

  defstruct compact: true, schema: 0

  test "nil format family" do
    Enum.map([
      {:ok, :nil},                    # <<0xc0>>
    ], &do_test/1)
  end

  test "bool format family" do
    Enum.map([
      {:ok, :false},                  # <<0xc2>>
      {:ok, :true},                   # <<0xc3>>
    ], &do_test/1)
  end

  test "int format family" do
    Enum.map([
      # zero
      {:ok, 0},                       # <<0x00>>
      # positive
      {:ok, 1},                       # <<0x01>>
      {:ok, 127},                     # <<0x7f>>
      {:ok, 128},                     # <<0xcc,0x80>>
      {:ok, 255},                     # <<0xcc,0xff>>
      {:ok, 256},                     # <<0xcd,0x01,0x00>>
      {:ok, 65535},                   # <<0xcd,0xff,0xff>>
      {:ok, 65536},                   # <<0xce,0x00,0x01,0x00,0x00>>
      {:ok, 4294967295},              # <<0xce,0xff,0xff,0xff,0xff>>
      {:ok, 4294967296},              # <<0xcf,0x00,0x00,0x00,0x01,0x00,0x00,0x00,0x00>>
      {:ok, 18446744073709551615},    # <<0xcf,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff>>
      {:error, 18446744073709551616},
      # negative
      {:ok, -1},                      # <<0xff>>
      {:ok, -32},                     # <<0xe0>>
      {:ok, -33},                     # <<0xd0,0xdf>>
      {:ok, -128},                    # <<0xd0,0x80>>
      {:ok, -129},                    # <<0xd1,0xff,0x7f>>
      {:ok, -32768},                  # <<0xd1,0x80,0x00>>
      {:ok, -32769},                  # <<0xd2,0xff,0xff,0x7f,0xff>>
      {:ok, -2147483648},             # <<0xd2,0x80,0x00,0x00,0x00>>
      {:ok, -2147483649},             # <<0xd3,0xff,0xff,0xff,0xff,0x7f,0xff,0xff,0xff>>
      {:ok, -9223372036854775808},    # <<0xd3,0x80,0x00,0x00,0x00,0x00,0x00,0x00,0x00>>
      {:error, -9223372036854775809},
    ], &do_test/1)
  end

  test "float format family" do
    Enum.map([
      # zero
      {:ok,  0.0},                    # <<0xcb,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00>>
      # single : positive
      {:ok,  2.802597e-45},           # <<0xcb,0x36,0xb0,0x00,0x00,0x06,0xd5,0x81,0x81>>
      {:ok,  3.402823e+38},           # <<0xcb,0x47,0xef,0xff,0xff,0x96,0x6a,0xd9,0x24>>
      # single : negative
      {:ok, -3.402823e+38},           # <<0xcb,0xc7,0xef,0xff,0xff,0x96,0x6a,0xd9,0x24>>
      {:ok, -2.802597e-45},           # <<0xcb,0xb6,0xb0,0x00,0x00,0x06,0xd5,0x81,0x81>>
      # double : positive
      {:ok,  4.940656458412465E-324}, # <<0xcb,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x01>>
      {:ok,  1.797693134862231E+308}, # <<0xcb,0x7f,0xef,0xff,0xff,0xff,0xff,0xfe,0x57>>
      # double : negative
      {:ok, -1.797693134862231E+308}, # <<0xcb,0xff,0xef,0xff,0xff,0xff,0xff,0xfe,0x57>>
      {:ok, -4.940656458412465E-324}, # <<0xcb,0x80,0x00,0x00,0x00,0x00,0x00,0x00,0x01>>
    ], &do_test/1)
    Enum.map([
      {:ok, 1.0, <<0xca, 1.0 :: 32-big-float>>}, # 32bit -> 64bit
    ], &do_decode/1)
  end

  test "str format family" do
    hex = to_hex(get_bin())
    parts = %{
      0 => binary_part(hex, 0, 255),
      1 => binary_part(hex, 0, 256),
      2 => binary_part(hex, 0, 65535),
      3 => binary_part(hex, 0, 65536),
    }
    Enum.map([
      {:ok, :compact},                # <<0xa7,0x63,0x6f,0x6d,0x70,0x61,0x63,0x74>>
      {:ok, :schema},                 # <<0xa6,0x73,0x63,0x68,0x65,0x6d,0x61>>
      {:ok, :erlang.binary_to_atom(parts[0], :utf8)}, # 255 (limit:atom)
    ], &do_test/1)
    Enum.map([
      {:ok, <<0x5f,0x5f,0x5f,0x5f>>, <<0xa4,0x5f,0x5f,0x5f,0x5f>>},
      {:ok, parts[1], <<0xda, 0x01,0x00, parts[1] :: binary>>},
      {:ok, parts[2], <<0xda, 0xff,0xff, parts[2] :: binary>>},
      {:ok, parts[3], <<0xdb, 0x00,0x01,0x00,0x00, parts[3] :: binary>>}
      # 536870911 (limit:32bit)
      # 4294967295
      # 4294967296
      # 2305843009213693951 (limit:64bit)
    ], &do_decode/1)
  end

  test "bin format family" do
    bin = get_bin()
    Enum.map([
      {:ok, <<>>},
      {:ok, binary_part(bin, 0, 255)},
      {:ok, binary_part(bin, 0, 256)},
      {:ok, binary_part(bin, 0, 65535)},
      {:ok, binary_part(bin, 0, 65536)},
      # 536870911 (limit:32bit)
      # 4294967295
      # 4294967296
      # 2305843009213693951 (limit:64bit)
      {:error, <<0 :: 7>>},
    ], &do_test/1)
    Enum.map([
      {:error, <<0 :: 7>>},
    ], &do_decode/1)
  end

  test "array format family" do
    bin = get_bin()
    Enum.map([
      {:ok, []},                      # <<0x90>>
      {:ok, [1]},                     # <<0x91,0x01>>
      {:ok, [1, 2]},                  # <<0x92,0x01,0x02>>
      {:ok, [1, 2, [3]]},             # <<0x93,0x01,0x02,0x91,0x03>>
      {:ok, [1, :ok, "ok"]},          # <<0x93,0x01,0xa2,0x6f,0x6b,0xc4,0x02,0x6f,0x6b>>
      {:ok, :erlang.binary_to_list(binary_part(bin, 0, 255))},
      {:ok, :erlang.binary_to_list(binary_part(bin, 0, 256))},
      {:ok, :erlang.binary_to_list(binary_part(bin, 0, 65535))},
      {:ok, :erlang.binary_to_list(binary_part(bin, 0, 65536))}
      # 4294967295
      # 4294967296
    ], &do_test/1)
    Enum.map([
      {:error, <<0xc1>>, <<0x91,0xc1>>}
    ], &do_decode/1)
  end

  test "map format family" do
    parts = %{
      0 => :lists.seq(1, 255),
      1 => :lists.seq(1, 256),
      2 => :lists.seq(1, 65535),
      3 => :lists.seq(1, 65536),
    }
    Enum.map([
      {:ok, %{}},                     # <<0x80>>
      {:ok, %{:compact => true}},     # <<0x81,0xa7,0x63,0x6f,0x6d,0x70,0x61,0x63,0x74,0xc3>>
      {:ok, %{:map => %{} }},         # <<0x81,0xa3,0x6d,0x61,0x70,0x80>>
      {:ok, :maps.from_list(:lists.zip(parts[0], parts[0]))},
      {:ok, :maps.from_list(:lists.zip(parts[1], parts[1]))},
      {:ok, :maps.from_list(:lists.zip(parts[2], parts[2]))},
      {:ok, :maps.from_list(:lists.zip(parts[3], parts[3]))}
      # 4294967295
      # 4294967296
    ], &do_test/1)
    Enum.map([
      {:error, <<0xc1>>, <<0x81,0xc1>>},
      {:error, <<0xc1>>, <<0x81,0xa3,0x6d,0x61,0x70,0x81,0xc1>>},
    ], &do_decode/1)
  end

  test "map format family / struct" do
    Enum.map([
      {:ok, %MessagepackTest{:compact => false, :schema => -1}},
    ], &do_test/1)
  end

  test "ext format family / function" do
    Enum.map([
      {:ok, &do_test/1},              # 10.23: NEW_FUN_EXT(112) = 92
      {:ok, &:erlang.fun_info/1},     # 10.24: EXPORT_EXT(113) = 27
    ], &do_test/1)
  end

  test "ext format family / pid" do
    Enum.map([
      {:ok, self()},                  # 10.10: PID_EXT(103) = 30
    ], &do_test/1)
  end

  test "ext format family / port" do
    Enum.map([
      {:ok, hd(:erlang.ports())},     # 10.9 : PORT_EXT(102) = 26
    ], &do_test/1)

    Enum.map([
    ], &do_decode/1)
  end

  test "ext format family / reference" do
    Enum.map([
      {:ok, make_ref()},              # 10.20: NEW_REFERENCE_EXT(114) = 36
    ], &do_test/1)
  end

  test "ext format family / tuple" do
    bin = get_bin()
    Enum.map([
      {:ok, {}},                      # <<0xc7,0x00,0x05>> < 255
      {:ok, {:true}},                 # <<0xd4,0x05,0xc3>>
      {:ok, {:true, :false}},         # <<0xd5,0x05,0xc3,0xc2>>
      {:ok, {:true, :ok}},            # <<0xd6,0x05,0xc3,0xa2,0x6f,0x6b>>
      {:ok, {:true, :false, :error}}, # <<0xd7,0x05,0xc3,0xc2,0xa5,0x65,0x72,0x72,0x6f,0x72>>
      {:ok, {:true, :error, 0.0}},    # <<0xd8,0x05,0xc3,0xa5,...,0xcb,...>>
      {:ok, :erlang.list_to_tuple(:erlang.binary_to_list(binary_part(bin, 0, 256)))},
      {:ok, :erlang.list_to_tuple(:erlang.binary_to_list(binary_part(bin, 0, 65535)))},
      {:ok, :erlang.list_to_tuple(:erlang.binary_to_list(binary_part(bin, 0, 65536)))}
      # 16777215 (limit?, TODO)
      # 67108863 (limit)
    ], &do_test/1)

    Enum.map([
      {:error, <<0xc1>>, <<0xd4,0x05,0xc1>>},
    ], &do_decode/1)
  end

  test "ext format family (composite)" do
    port = hd(:erlang.ports())
    Enum.map([
      {:ok, [make_ref(), port, &:erlang.port_info/1, :erlang.port_info(port)]}
    ], &do_test/1)
  end

  test "never used" do
    Enum.map([
      {:error, <<0xc1>>},
    ], &do_decode/1)
  end

  ## -- internal --

  defp do_decode({:ok, term, binary}) do
    assert(term === Messagepack.decode!(binary))
  end

  defp do_decode({:error, binary}) do
    error = catch_error(Messagepack.decode!(binary))
    assert(binary === error.value)
  end

  defp do_decode({:error, term, binary}) do
    error = catch_error(Messagepack.decode!(binary))
    assert(term === error.value)
  end

  defp do_test({:ok, term}) do
    assert(term === Messagepack.decode!(Messagepack.encode!(term)))
  end

  defp do_test({:error, term}) do
    error = catch_error(Messagepack.encode!(term))
    assert(term === error.value)
  end

  defp get_bin() do
    elem(:code.get_object_code(:elixir_parser), 1) # 1.3.0-dev : 691200
  end

  defp to_hex(bin) do
    for <<x :: 4 <- bin>>, into: "", do: if(x < 10, do: <<x + ?0>>, else: <<x - 10 + ?a>>)
  end

end
