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

with Ada.Integer_Text_IO;
with Ada.Text_IO; use Ada.Text_IO;

with Ada.Streams;

with AWS.Net.Buffered;
with AWS.HPACK;

package body AWS.HTTP2.Frame is

   use Ada;
   use Ada.Streams;
   use System;

   pragma Warnings (Off, "overlay changes scalar storage order");

   Connection_Preface : constant Stream_Element_Array :=
                          (16#50#, 16#52#, 16#49#, 16#20#, 16#2a#, 16#20#,
                           16#48#, 16#54#, 16#54#, 16#50#, 16#2f#, 16#32#,
                           16#2e#, 16#30#, 16#0d#, 16#0a#, 16#0d#, 16#0a#,
                           16#53#, 16#4d#, 16#0d#, 16#0a#, 16#0d#, 16#0a#);

   --  RFC-7540 6.2
   --
   --  +---------------+
   --  |Pad Length? (8)|
   --  +-+-------------+-----------------------------------------------+
   --  |E|                 Stream Dependency? (31)                     |
   --  +-+-------------+-----------------------------------------------+
   --  |  Weight? (8)  |
   --  +-+-------------+-----------------------------------------------+
   --  |                   Header Block Fragment (*)                 ...
   --  +---------------------------------------------------------------+
   --  |                           Padding (*)                       ...
   --  +---------------------------------------------------------------+

   type Headers_Padded_Payload is record
      Pad_Length : Byte_1;
   end record;

   for Headers_Padded_Payload'Bit_Order use High_Order_First;
   for Headers_Padded_Payload'Scalar_Storage_Order use High_Order_First;
   for Headers_Padded_Payload use record
      Pad_Length at 0 range 0 .. 7;
   end record;

   --  RFC-7540 6.5.1
   --
   --  +-------------------------------+
   --  |       Identifier (16)         |
   --  +-------------------------------+-------------------------------+
   --  |                        Value (32)                             |
   --  +---------------------------------------------------------------+

   type Settings_Payload is record
      Id    : Settings_Kind;
      Value : Byte_4;
   end record;

   for Settings_Payload'Bit_Order use High_Order_First;
   for Settings_Payload'Scalar_Storage_Order use High_Order_First;
   for Settings_Payload use record
      Id    at 0 range 0 .. 15;
      Value at 2 range 0 .. 31;
   end record;

   --  RFC-7540 6.9
   --
   --  +-+-------------------------------------------------------------+
   --  |R|              Window Size Increment (31)                     |
   --  +-+-------------------------------------------------------------+

   type Window_Update_Payload is record
      R              : Bit_1;
      Size_Increment : Byte_4 range 0 .. 2 ** 31 - 1;
   end record;

   for Window_Update_Payload'Bit_Order use High_Order_First;
   for Window_Update_Payload'Scalar_Storage_Order use High_Order_First;
   for Window_Update_Payload use record
      R              at 0 range 31 .. 31;
      Size_Increment at 0 range  0 .. 30;
   end record;

   --  The header payload present only if Priority flag is set

   --  RFC-7540 6.3
   --
   --  +-+-------------------------------------------------------------+
   --  |E|                  Stream Dependency (31)                     |
   --  +-+-------------+-----------------------------------------------+
   --  |   Weight (8)  |
   --  +-+-------------+

   type Headers_Priority_Payload is record
      E                 : Bit_1;
      Stream_Dependency : Byte_4 range 0 .. 2 ** 31 - 1;
      Weight            : Byte_1;
   end record;

   for Headers_Priority_Payload'Bit_Order use High_Order_First;
   for Headers_Priority_Payload'Scalar_Storage_Order use High_Order_First;
   for Headers_Priority_Payload use record
      E                 at 0 range 31 .. 31;
      Stream_Dependency at 0 range  0 .. 30;
      Weight            at 4 range  0 ..  8;
   end record;

   --  RFC-7540 6.1
   --
   --  +---------------+
   --  |Pad Length? (8)|
   --  +---------------+-----------------------------------------------+
   --  |                            Data (*)                         ...
   --  +---------------------------------------------------------------+
   --  |                           Padding (*)                       ...
   --  +---------------------------------------------------------------+

   type Data_Payload is record
      Pad_Length : Byte_1;
   end record;

   for Data_Payload'Bit_Order use High_Order_First;
   for Data_Payload'Scalar_Storage_Order use High_Order_First;
   for Data_Payload use record
      Pad_Length at 0 range 0 .. 7;
   end record;

   --  RFC-7540 6.4
   --
   --  +---------------------------------------------------------------+
   --  |                        Error Code (32)                        |
   --  +---------------------------------------------------------------+

   type RST_Stream_Payload is record
      Error_Code : Byte_4;
   end record;

   for RST_Stream_Payload'Bit_Order use High_Order_First;
   for RST_Stream_Payload'Scalar_Storage_Order use High_Order_First;
   for RST_Stream_Payload use record
      Error_Code at 0 range 0 .. 31;
   end record;

   procedure Create (Kind : Kind_Type; Flags : Flags_Type) is null;

   procedure Dump (Msg : String; S : Stream_Element_Array) is
   begin
      Put ("(" & Msg & ":  ");
      for V of S loop
         Put (Utils.Hex (Integer (V), Width => 2) & ' ');
      end loop;
      Put_Line (")");
   end Dump;

   procedure Dump (O : Object) is
   begin
      Put ("FRAME: Id:" & Integer (O.H.Stream_Id)'Img);
      Put ("  L:" & Integer (O.H.Length)'Img);
      Integer_Text_IO.Put (Integer (O.H.Length), Width => 6, Base => 16);
      Put_Line ("  Kind: " & O.H.Kind'Img & "   Flags:" & O.H.Flags'Img);
   end Dump;

   procedure Dump_Payload (Sock : Net.Socket_Type'Class; O : Object) is

      L : constant Stream_Element_Offset := Stream_Element_Offset (O.H.Length);

      procedure Dump_Settings is
         -- RFC-7540 / 6.5
         N : constant Natural := Natural (L) / (Settings_Payload'Size / 8);
         P : Settings_Payload;
         S : Stream_Element_Array (1 .. Settings_Payload'Size / 8)
               with Address => P'Address;
      begin
         for K in 1 .. N loop
            Net.Buffered.Read (Sock, S);
            Dump ("set", S);
            Put_Line ("S: " & P.Id'Img & " = " & P.Value'Img);
         end loop;
      end Dump_Settings;

      procedure Dump_Window_Update is
         -- RFC-7540 / 6.9
         P : Window_Update_Payload;
         S : Stream_Element_Array (1 .. 4) with Address => P'Address;
      begin
         Net.Buffered.Read (Sock, S);
         Dump ("wu", S);
         Put_Line ("S: " & P.Size_Increment'Img);
      end Dump_Window_Update;

      procedure Dump_Headers is
         -- RFC-7540 / 6.2
         Pad_PL : Headers_Padded_Payload := (Pad_Length => 0);
         Pad_S  : Stream_Element_Array (1 .. 1)
                    with Address => Pad_PL'Address;

         Prio_PL : Headers_Priority_Payload;
         Prio_S  : Stream_Element_Array (1 .. 5)
                     with Address => Prio_PL'Address;
      begin
         --  Read pad-length if corresponding flag set

         if (O.H.Flags and Padded_Flag) = Padded_Flag then
            Net.Buffered.Read (Sock, Pad_S);
            Put_Line ("H: Pad length " & Pad_PL.Pad_Length'Img);
         end if;

         --  Read header priority patload if corresponding flag set

         if (O.H.Flags and Priority_Flag) = Priority_Flag then
            Net.Buffered.Read (Sock, Prio_S);
            Put_Line ("H: stream deps & weight ");
         end if;

         if (O.H.Flags and End_Stream_Flag) = End_Stream_Flag then
            Put_Line (" END_STREAM");
         end if;

         if (O.H.Flags and End_Headers_Flag) = End_Headers_Flag then
            Put_Line (" END_HEADERS");
         end if;

         --  Read header block

         HPACK.Get_Headers (Sock, Stream_Element_Offset (O.H.Length));

         --  Read padding if any

         if Pad_PL.Pad_Length > 0 then
            declare
               Trash : Stream_Element_Array
                         (1 .. Stream_Element_Offset (Pad_PL.Pad_Length));
            begin
               Net.Buffered.Read (Sock, Trash);
            end;
         end if;
      end Dump_Headers;

   begin
      if O.H.Kind = Settings then
         Dump_Settings;

      elsif O.H.Kind = Window_Update then
         Dump_Window_Update;

      elsif O.H.Kind = Headers then
         Dump_Headers;

      else
         Put_Line ("=> YET unsupported frame, skip payload");
         declare
            S : Stream_Element_Array (1 .. Stream_Element_Offset (O.H.Length));
         begin
            Net.Buffered.Read (Sock, S);
         end;
      end if;
   end;

   function Settings return Object is
   begin
      return O : Object do
         O.H.Stream_Id := 0;
         O.H.Length := 6; -- for demo
         O.H.Kind := Settings;
         O.H.R := 0;
         O.H.Flags := 0; -- End_Stream_Flag;
      end return;
   end Settings;

   procedure Set_Payload (O : in out Object; Payload : String) is
   begin
      O.H.Length := Payload'Length;

      O.Payload := new
        Stream_Element_Array (1 .. Stream_Element_Offset (O.H.Length));

      for K in Payload'Range loop
         O.Payload (Stream_Element_Offset (K)) := Character'Pos (Payload (K));
      end loop;
   end Set_Payload;

   function Headers return Object is
      HPL : constant Stream_Element_Array := HPACK.Encode;
   begin
      return O : Object do
         O.H.Stream_Id := 1;
         O.H.Length := HPL'Length; -- for demo
         O.H.Kind := Headers;
         O.H.R := 0;
         O.H.Flags := End_Headers_Flag;

         --  Set_Payload (O, PL);
         O.Payload := new Stream_Element_Array'(HPL);

         O.H.Length := O.Payload.all'Length;
         Put_Line ("H: payload " & O.H.Length'Img);
      end return;
   end Headers;

   function E_Headers return Object is
   begin
      return O : Object do
         O.H.Stream_Id := 1;
         O.H.Length := 2; -- for demo
         O.H.Kind := Headers;
         O.H.R := 0;
         O.H.Flags := 0;

         --  Set_Payload (O, PL);
         O.Payload := new Stream_Element_Array'
                            (Character'Pos (ASCII.CR),
                             Character'Pos (ASCII.LF));

--         O.H.Length := O.Payload.all'Length;
         Put_Line ("H: payload " & O.H.Length'Img);
      end return;
   end E_Headers;

   function Data return Object is
      PL : constant String := "<p>Hello ! from AWS</p>";
   begin
      return O : Object do
         O.H.Stream_Id := 1;
         O.H.Length := PL'Length;
         O.H.Kind := Data;
         O.H.R := 0;
         O.H.Flags := End_Stream_Flag;

         Set_Payload (O, PL);
      end return;
   end Data;

   function RST return Object is
      EC : RST_Stream_Payload;
      S  : Stream_Element_Array (1 .. 4) with Address => EC'Address;
   begin
      return O : Object do
         O.H.Stream_Id := 0;
         O.H.Length := 4;
         O.H.Kind := RST_Stream;
         O.H.R := 0;
         O.H.Flags := 0;

         EC.Error_Code := 0;
         O.Payload := new Stream_Element_Array'(S);
      end return;
   end RST;

   ----------
   -- Read --
   ----------

   procedure Read (Sock : Net.Socket_Type'Class) is
      Preface : Stream_Element_Array (1 .. 24);
      O       : Object;
      S       : Stream_Element_Array (1 .. 9) with Address => O'Address;
   begin
      --  First connection start with a preface
      --  This check should not be there!!!
      Net.Buffered.Read (Sock, Preface);

      if Preface = Connection_Preface then
         Put_Line ("connection preface ok");
      else
         --  Should be a PROTOCOL_ERROR
         Put_Line ("connection preface NOT ok");
      end if;

      --  Get Frames

      for k in 1 .. 3 loop
         Net.Buffered.Read (Sock, S);
         Dump (O);
         Dump_Payload (Sock, O);

--         if K = 1 then
--            Send (Sock, Ack_Settings);
--         end if;
      end loop;

      --  Answer settings payload

      Send (Sock, Settings);
      declare
        SP : Settings_Payload;
        X  : Stream_Element_Array (1 .. 6) with Address => SP'Address;
      begin
        SP.Id := MAX_CONCURRENT_STREAMS;
        SP.Value := 326;
        Net.Buffered.Write (Sock, X);
      end;
      Net.Buffered.Flush (Sock);

      Put_Line ("@@@ Try answering");

      --  Try sending an answer

--      Send (Sock, Ack_Settings);

      for k in 4 .. 4 loop
         Net.Buffered.Read (Sock, S);
         Dump (O);
         Dump_Payload (Sock, O);
      end loop;

      Send (Sock, Ack_Settings);

      Send (Sock, Headers);
--      Send (Sock, E_Headers);

      Send (Sock, Data);

--      Send (Sock, RST);
      delay 5.0;
   end Read;

   function Ack_Settings return Object is
   begin
      return O : Object do
         --  A settings frame must always have a stream id of 0
         O.H.Stream_Id := 0;
         O.H.Length := 0;
         O.H.Kind := Settings;
         O.H.R := 0;
         O.H.Flags := Ack_Flag; -- + End_Stream_Flag;
      end return;
   end Ack_Settings;

   procedure Send (Sock : Net.Socket_Type'Class; O : Object) is
      use type Utils.Stream_Element_Array_Access;
      S : Stream_Element_Array (1 .. 9) with Address => O'Address;
   begin
      Dump ("send", S);
      Net.Buffered.Write (Sock, S);
      Net.Buffered.Flush (Sock);

      if O.Payload /= null then
         Put_Line ("==============> SEND some payload " & O.Payload'Length'Img);
         Net.Buffered.Write (Sock, O.Payload.all);
      end if;

      Net.Buffered.Flush (Sock);
   end Send;

end AWS.HTTP2.Frame;
