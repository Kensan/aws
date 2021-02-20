------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                      Copyright (C) 2021, AdaCore                         --
--                                                                          --
--  This library is free software;  you can redistribute it and/or modify   --
--  it under terms of the  GNU General Public License  as published by the  --
--  Free Software  Foundation;  either version 3,  or (at your  option) any --
--  later version. This library is distributed in the hope that it will be  --
--  useful, but WITHOUT ANY WARRANTY;  without even the implied warranty of --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                    --
--                                                                          --
--  As a special exception under Section 7 of GPL version 3, you are        --
--  granted additional permissions described in the GCC Runtime Library     --
--  Exception, version 3.1, as published by the Free Software Foundation.   --
--                                                                          --
--  You should have received a copy of the GNU General Public License and   --
--  a copy of the GCC Runtime Library Exception along with this program;    --
--  see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see   --
--  <http://www.gnu.org/licenses/>.                                         --
--                                                                          --
--  As a special exception, if other files instantiate generics from this   --
--  unit, or you link this unit with other files to produce an executable,  --
--  this  unit  does not  by itself cause  the resulting executable to be   --
--  covered by the GNU General Public License. This exception does not      --
--  however invalidate any other reasons why the executable file  might be  --
--  covered by the  GNU Public License.                                     --
------------------------------------------------------------------------------

with System;

with AWS.Net;
with AWS.Utils;

package AWS.HTTP2.Frame is

   type Object is private;

   type Kind_Type is (Data, Headers, Priority,
                      RST_Stream, Settings, Push_Promise,
                      Ping, GoAway, Window_Update, Continuation)
     with Size => 8;
   --  Frame kind, see section 6 RFC 7540

   type Error_Codes is
     (No_Error, Protocol_Error, Internal_Error, Flow_Control_Error,
      Settings_Timeout, Stream_Closed, Frame_Size_Error, Refused_Stream,
      Cancel, Compression_Error, Connect_Error, Enhance_Your_CALM,
      Inadequate_Security, HTTP_1_1_Required);

   --  Error codes that are used in RST_Stream and GoAway frames

   type Flags_Type is mod 2 ** 8 with Size => 8;

   End_Stream_Flag  : constant Flags_Type;
   Ack_Flag         : constant Flags_Type;
   End_Headers_Flag : constant Flags_Type;
   Padded_Flag      : constant Flags_Type;
   Priority_Flag    : constant Flags_Type;

   type Settings_Kind is
     (HEADER_TABLE_SIZE,
      ENABLE_PUSH,
      MAX_CONCURRENT_STREAMS,
      INITIAL_WINDOW_SIZE,
      MAX_FRAME_SIZE,
      MAX_HEADER_LIST_SIZE);

   procedure Create (Kind : Kind_Type; Flags : Flags_Type);

   function Ack_Settings return Object;

   function Settings return Object;

   procedure Read (Sock : Net.Socket_Type'Class);

   procedure Send (Sock : Net.Socket_Type'Class; O : Object);

private

   End_Stream_Flag  : constant Flags_Type := 16#01#;
   Ack_Flag         : constant Flags_Type := 16#01#;
   End_Headers_Flag : constant Flags_Type := 16#04#;
   Padded_Flag      : constant Flags_Type := 16#08#;
   Priority_Flag    : constant Flags_Type := 16#20#;

   for Kind_Type use (Data          => 16#0#,
                      Headers       => 16#1#,
                      Priority      => 16#2#,
                      RST_Stream    => 16#3#,
                      Settings      => 16#4#,
                      Push_Promise  => 16#5#,
                      Ping          => 16#6#,
                      GoAway        => 16#7#,
                      Window_Update => 16#8#,
                      Continuation  => 16#9#);

   for Error_Codes use (No_Error            => 16#0#,
                        Protocol_Error      => 16#1#,
                        Internal_Error      => 16#2#,
                        Flow_Control_Error  => 16#3#,
                        Settings_Timeout    => 16#4#,
                        Stream_Closed       => 16#5#,
                        Frame_Size_Error    => 16#6#,
                        Refused_Stream      => 16#7#,
                        Cancel              => 16#8#,
                        Compression_Error   => 16#9#,
                        Connect_Error       => 16#A#,
                        Enhance_Your_CALM   => 16#B#,
                        Inadequate_Security => 16#C#,
                        HTTP_1_1_Required   => 16#D#);

   for Settings_Kind use (HEADER_TABLE_SIZE      => 16#1#,
                          ENABLE_PUSH            => 16#2#,
                          MAX_CONCURRENT_STREAMS => 16#3#,
                          INITIAL_WINDOW_SIZE    => 16#4#,
                          MAX_FRAME_SIZE         => 16#5#,
                          MAX_HEADER_LIST_SIZE   => 16#6#);

   type Bit_1 is mod 2 ** 1 with Size => 1;

   type Byte_1 is mod 2 **  8 with Size => 8;
   type Byte_2 is mod 2 ** 16 with Size => 16;
   type Byte_3 is mod 2 ** 24 with Size => 24;
   type Byte_4 is mod 2 ** 32 with Size => 32;

   subtype Length_Type is Byte_3 range 0 .. 2 ** 24 - 1;

   type Header is record
      Length    : Length_Type;
      Kind      : Kind_Type;
      Flags     : Flags_Type;
      R         : Bit_1;
      Stream_Id : Natural range 0 .. 2 ** 31 - 1;
   end record
     with Dynamic_Invariant =>
            (if Kind = Window_Update then Length = 4)
            and then
            (if Kind = Headers then Stream_Id > 0);

   --  +-----------------------------------------------+
   --  |                 Length (24)                   |
   --  +---------------+---------------+---------------+
   --  |   Type (8)    |   Flags (8)   |
   --  +-+-------------+---------------+-------------------------------+
   --  |R|                 Stream Identifier (31)                      |
   --  +=+=============================================================+
   --  |                   Frame Payload (0...)                      ...
   --  +---------------------------------------------------------------+

   for Header'Bit_Order use System.High_Order_First;
   for Header'Scalar_Storage_Order use System.High_Order_First;
   for Header use record
      Length    at 0 range 0 .. 23;
      Kind      at 0 range 24 .. 31;
      Flags     at 4 range 0 .. 7;
      R         at 5 range 0 .. 0;
      Stream_Id at 5 range 1 .. 31;
   end record;

   type Object is record
      H       : Header;
      Payload : Utils.Stream_Element_Array_Access;
   end record;

end AWS.HTTP2.Frame;
