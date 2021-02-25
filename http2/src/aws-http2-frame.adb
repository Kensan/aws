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

with AWS.Headers;
with AWS.Net.Buffered;
with AWS.HPACK;
with AWS.HTTP2.Frame.Data;
with AWS.HTTP2.Frame.Headers;
with AWS.HTTP2.Frame.Settings;
with AWS.HTTP2.Frame.Window_Update;

package body AWS.HTTP2.Frame is

   use Ada;
   use Ada.Streams;
   use System;

   pragma Warnings (Off, "overlay changes scalar storage order");

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

   procedure Dump (O : Object'Class) is
   begin
      Put ("FRAME: Id:" & Integer (O.Header.H.Stream_Id)'Img);
      Put ("  L:" & Integer (O.Header.H.Length)'Img);
      Integer_Text_IO.Put (Integer (O.Header.H.Length), Width => 8, Base => 16);
      Put_Line ("  Kind: " & O.Header.H.Kind'Img & "   Flags:" & O.Header.H.Flags'Img);
   end Dump;

   procedure Dump_Payload (Sock : Net.Socket_Type'Class; O : Object'Class) is

      L : constant Stream_Element_Count :=
            Stream_Element_Offset (O.Header.H.Length);

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

         if (O.Header.H.Flags and Padded_Flag) = Padded_Flag then
            Net.Buffered.Read (Sock, Pad_S);
            Put_Line ("H: Pad length " & Pad_PL.Pad_Length'Img);
         end if;

         --  Read header priority patload if corresponding flag set

         if (O.Header.H.Flags and Priority_Flag) = Priority_Flag then
            Net.Buffered.Read (Sock, Prio_S);
            Put_Line ("H: stream deps & weight ");
         end if;

         if (O.Header.H.Flags and End_Stream_Flag) = End_Stream_Flag then
            Put_Line (" END_STREAM");
         end if;

         if (O.Header.H.Flags and End_Headers_Flag) = End_Headers_Flag then
            Put_Line (" END_HEADERS");
         end if;

         --  Read header block

         HPACK.Get_Headers (Sock, Stream_Element_Offset (O.Header.H.Length));

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

      use Utils;

   begin
      if O.Header.H.Kind = K_Settings then
         Settings.Dump (Settings.Object (O));

      elsif O.Header.H.Kind = K_Window_Update then
         Window_Update.Dump (Window_Update.Object (O));

      elsif O.Header.H.Kind = K_Headers then
         Headers.Dump (Headers.Object (O));

      else
         Put_Line ("=> YET unsupported frame, skip payload");
         declare
            S : Stream_Element_Array (1 .. Stream_Element_Offset (O.Header.H.Length));
         begin
            Net.Buffered.Read (Sock, S);
         end;
      end if;
   end Dump_Payload;

   procedure Set_Payload (O : in out Object; Payload : String) is
   begin
      O.Header.H.Length := Payload'Length;

      O.Payload := new
        Stream_Element_Array (1 .. Stream_Element_Offset (O.Header.H.Length));

      for K in Payload'Range loop
         O.Payload (Stream_Element_Offset (K)) := Character'Pos (Payload (K));
      end loop;
   end Set_Payload;

   function RST return Object is
      EC : RST_Stream_Payload;
      S  : Stream_Element_Array (1 .. 4) with Address => EC'Address;
   begin
      return O : Object do
         O.Header.H.Stream_Id := 0;
         O.Header.H.Length := 4;
         O.Header.H.Kind := RST_Stream;
         O.Header.H.R := 0;
         O.Header.H.Flags := 0;

         EC.Error_Code := 0;
         O.Payload := new Stream_Element_Array'(S);
      end return;
   end RST;

   ----------
   -- Read --
   ----------

   procedure Go (Sock : Net.Socket_Type'Class) is
      O : Object;
   begin
      --  Get Frames

      for k in 1 .. 3 loop
         Put_Line ("FFFFFFFFF " & K'Img);
         declare
            F : Object'Class := Read (Sock);
         begin
            Dump (F);
            Dump_Payload (Sock, F);
         end;
      end loop;

      --  Answer settings payload

      declare
        SP : Settings.Payload;
      begin
        SP.Id := Settings.MAX_CONCURRENT_STREAMS;
        SP.Value := 326;

        Send (Sock, Settings.Create (Settings.Set'(1 => SP)));
      end;
      Net.Buffered.Flush (Sock);

      Put_Line ("@@@ Try answering");

      --  Try sending an answer

      for k in 4 .. 4 loop
         declare
            F : Object'Class := Read (Sock);
         begin
            Dump (F);
            Dump_Payload (Sock, F);
         end;
      end loop;

      Send (Sock, Settings.Ack);

      Send (Sock, Frame.Headers.Create (AWS.Headers.Empty_List));

      Send (Sock, Frame.Data.Create ("<p>Hello ! from AWS</p>"));

      delay 5.0;
   end Go;

   function Read (Sock : Net.Socket_Type'Class) return Object'Class is
      H : Object;
   begin
      Net.Buffered.Read (Sock, H.Header.S);

      case H.Header.H.Kind is
         when K_Data =>
            return Frame.Data.Read (Sock, H);
         when K_Settings =>
            return Frame.Settings.Read (Sock, H);
         when K_Headers =>
            return Frame.Headers.Read (Sock, H);
         when K_Window_Update =>
            return Frame.Window_Update.Read (Sock, H);
         when Others =>
            return H;
      end case;
   end Read;

   procedure Send (Sock : Net.Socket_Type'Class; O : Object'Class) is
      use type Utils.Stream_Element_Array_Access;
   begin
      Dump ("send", O.Header.S);

      Net.Buffered.Write (Sock, O.Header.S);

      if O.Header.H.Length > 0 then
         Send_Payload (Sock, Object'Class (O));
      end if;

      Net.Buffered.Flush (Sock);
   end Send;

end AWS.HTTP2.Frame;
