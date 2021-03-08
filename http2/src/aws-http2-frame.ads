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

with Ada.Finalization;
with Ada.Streams;

with System;

with AWS.Net;
with AWS.Utils;

package AWS.HTTP2.Frame is

   use Ada;
   use Ada.Streams;

   type Object is new Finalization.Controlled with private;

   type Kind_Type is (K_Data, k_Headers, K_Priority,
                      K_RST_Stream, K_Settings, Push_Promise,
                      Ping, GoAway, K_Window_Update, Continuation)
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

   function Read (Sock : Net.Socket_Type'Class) return Object'Class;

   procedure Send (Sock : Net.Socket_Type'Class; O : Object'Class);

   procedure Send_Payload (Sock : Net.Socket_Type'Class; O : Object) is null;

   procedure Go (Sock : Net.Socket_Type'Class);

private

   use Ada;

   End_Stream_Flag  : constant Flags_Type := 16#01#;
   Ack_Flag         : constant Flags_Type := 16#01#;
   End_Headers_Flag : constant Flags_Type := 16#04#;
   Padded_Flag      : constant Flags_Type := 16#08#;
   Priority_Flag    : constant Flags_Type := 16#20#;

   for Kind_Type use (K_Data          => 16#0#,
                      k_Headers       => 16#1#,
                      K_Priority      => 16#2#,
                      K_RST_Stream    => 16#3#,
                      K_Settings      => 16#4#,
                      Push_Promise  => 16#5#,
                      Ping          => 16#6#,
                      GoAway        => 16#7#,
                      K_Window_Update => 16#8#,
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

   subtype Length_Type is Byte_3 range 0 .. 2 ** 24 - 1;

   type Header is record
      Length    : Length_Type;
      Kind      : Kind_Type;
      Flags     : Flags_Type;
      R         : Bit_1;
      Stream_Id : HTTP2.Stream_Id;
   end record
     with Dynamic_Predicate =>
            (if Kind = K_Window_Update then Length = 4)
              and then
            (if Kind = K_Headers then Stream_Id > 0)
              and then
            R = 0;

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

   type Header_View (Flat : Boolean := False) is record
      case Flat is
         when False => H : Header;
         when True  => S : Stream_Element_Array (1 .. Header'Size / 8);
      end case;
   end record with Unchecked_Union;

   type Object is new Finalization.Controlled with record
      Header  : Header_View;
      Payload : Utils.Stream_Element_Array_Access;
   end record;

   --  Shared payload object in Data and Headers

   type Padding is record
      Pad_Length : Byte_1;
   end record;

   for Padding'Bit_Order use System.High_Order_First;
   for Padding'Scalar_Storage_Order use System.High_Order_First;
   for Padding use record
      Pad_Length at 0 range 0 .. 7;
   end record;

end AWS.HTTP2.Frame;
