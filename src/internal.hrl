%% == define ==
                                                %
-define(I6_MIN,  (-1 bsl ( 6 - 1))).            % -32
-define(I8_MIN,  (-1 bsl ( 8 - 1))).            % -128
-define(I16_MIN, (-1 bsl (16 - 1))).            % -32768
-define(I32_MIN, (-1 bsl (32 - 1))).            % -2147483648
-define(I64_MIN, (-1 bsl (64 - 1))).            %  -9223372036854775808

-define(U4_MAX,  (1 bsl  4 - 1)).               % 15
-define(U5_MAX,  (1 bsl  5 - 1)).               % 31
-define(U7_MAX,  (1 bsl  7 - 1)).               % 127
-define(U8_MAX,  (1 bsl  8 - 1)).               % 255
-define(U16_MAX, (1 bsl 16 - 1)).               % 65535
-define(U32_MAX, (1 bsl 32 - 1)).               % 4294967295
-define(U64_MAX, (1 bsl 64 - 1)).               % 18446744073709551615

-define(EXT_FUNCTION,  1).
-define(EXT_PID,       2).
-define(EXT_PORT,      3).
-define(EXT_REFERENCE, 4).
-define(EXT_TUPLE,     5).

%% == type ==

-type(keyword() :: term()).
