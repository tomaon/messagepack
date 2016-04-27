defmodule MessagepackTest do
  use ExUnit.Case, async: true

  alias Messagepack

  defstruct compact: true, schema: 0

  test "nil format family" do
    Enum.map([
      {:ok, :nil},   # <<0xc0>>
    ], &do_test/1)
  end

  test "bool format family" do
    Enum.map([
      {:ok, :false}, # <<0xc2>>
      {:ok, :true},  # <<0xc3>>
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

    f = 1.0
    assert(f === do_decode(<<0xca, f :: 32-big-float>>))
  end

  test "str format family" do
    Enum.map([
      {:ok, :compact}, # <<0xa7,0x63,0x6f,0x6d,0x70,0x61,0x63,0x74>>
      {:ok, :schema},  # <<0xa6,0x73,0x63,0x68,0x65,0x6d,0x61>>
      # 255
      # 256
      # 65535
      # 65536
      # 4294967295
      # 4294967296
    ], &do_test/1)
  end

  test "bin format family" do
    bin = elem(:code.get_object_code(:elixir_parser), 1) # 1.3.0-dev : 691200
    Enum.map([
      {:ok, <<>>},
      {:ok, binary_part(bin,   0, 255)},
      {:ok, binary_part(bin, 100, 256)},
      {:ok, binary_part(bin, 200, 65535)},
      {:ok, binary_part(bin, 300, 65536)},
      # 4294967295
      # 4294967296
      {:error, <<0 :: 7>>},

    ], &do_test/1)
  end

  test "array format family" do
    Enum.map([
      {:ok, []},             # <<0x90>>
      {:ok, [1]},            # <<0x91,0x01>>
      {:ok, [1, 2]},         # <<0x92,0x01,0x02>>
      {:ok, [1, 2, [3]]},    # <<0x93,0x01,0x02,0x91,0x03>>
      {:ok, [1, :ok, "ok"]}, # <<0x93,0x01,0xa2,0x6f,0x6b,0xc4,0x02,0x6f,0x6b>>
      # 65535
      # 65536
      # 4294967295
      # 4294967296
    ], &do_test/1)
  end

  test "map format family" do
    Enum.map([
      {:ok, %{}},
      {:ok, %{:compact => true}},
      {:ok, %{:map => %{} }},
        # 65535
      # 65536
      # 4294967295
      # 4294967296
    ], &do_test/1)
  end

  test "struct" do
    Enum.map([
      {:ok, %MessagepackTest{:compact => false, :schema => -1}}
    ], &do_test/1)
  end

  test "ext format family" do
    # @see http://erlang.org/doc/apps/erts/erl_ext_dist.html, node=nonode@nohost
    ports = :erlang.ports()
    ref = make_ref()
    Enum.map([
      # function
      {:ok, &do_test/1},          # 10.23: NEW_FUN_EXT(112) = 92
      {:ok, &:erlang.fun_info/1}, # 10.24: EXPORT_EXT(113) = 27
      # pid
      {:ok, self()},              # 10.10: PID_EXT(103) = 30
      # port
      {:ok, hd(ports)},           # 10.9 : PORT_EXT(102) = 26
      # reference
      {:ok, ref},                 # 10.20: NEW_REFERENCE_EXT(114) = 36
      # tuple
      {:ok, {}},
      {:ok, {:true}},
      {:ok, {:true, :false}},
      {:ok, {:true, :ok}},
      {:ok, {:true, :false, :error}},
      {:ok, {:true, :error, 0.0}},
      # 65535
      # 65536
      # 4294967295
      # 4294967296
      {:ok, [ref, &:erlang.port_info/1, ports, :erlang.port_info(hd(ports))]}
    ], &do_test/1)
  end

  ## -- internal --

  defp do_decode(binary) do
    Messagepack.decode!(binary)
  end

  defp do_test({:ok, term}) do
    assert(term === Messagepack.decode!(Messagepack.encode!(term)))
  end

  defp do_test({:error, term}) do
    error = catch_error(Messagepack.encode!(term))
    assert(term === error.value)
  end

end
