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

package body AWS.HTTP2.Frame is

   use Ada;
   use Ada.Streams;

   pragma Warnings (Off, "overlay changes scalar storage order");

   Connection_Preface : constant Stream_Element_Array :=
                          (16#50#, 16#52#, 16#49#, 16#20#, 16#2a#, 16#20#,
                           16#48#, 16#54#, 16#54#, 16#50#, 16#2f#, 16#32#,
                           16#2e#, 16#30#, 16#0d#, 16#0a#, 16#0d#, 16#0a#,
                           16#53#, 16#4d#, 16#0d#, 16#0a#, 16#0d#, 16#0a#);


   type Settings_Payload is record
      Id    : Byte_2;
      Value : Byte_4;
   end record;

   for Settings_Payload'Bit_Order use System.High_Order_First;
   for Settings_Payload'Scalar_Storage_Order use System.High_Order_First;
   for Settings_Payload use record
      Id    at 0 range 0 .. 15;
      Value at 2 range 0 .. 31;
   end record;

   type Window_Update_Payload is record
      R              : Bit_1;
      Size_Increment : Byte_4 range 0 .. 2 ** 31 - 1;
   end record;

   for Window_Update_Payload'Bit_Order use System.High_Order_First;
   for Window_Update_Payload'Scalar_Storage_Order use System.High_Order_First;
   for Window_Update_Payload use record
      R              at 0 range 31 .. 31;
      Size_Increment at 0 range  0 .. 30;
   end record;

   --  The header payload present only if Priority flag is set

   type Header_Priority_Payload is record
      E                 : Bit_1;
      Stream_Dependency : Byte_4 range 0 .. 2 ** 31 - 1;
      Weight            : Byte_1;
   end record;

   for Header_Priority_Payload'Bit_Order use System.High_Order_First;
   for Header_Priority_Payload'Scalar_Storage_Order use System.High_Order_First;
   for Header_Priority_Payload use record
      E                 at 0 range 31 .. 31;
      Stream_Dependency at 0 range  0 .. 30;
      Weight            at 4 range  0 ..  8;
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
      Put_Line ("Id : " & Integer (O.H.Id)'Img);
      Put ("   L    : " & Integer (O.H.Length)'Img);
      Integer_Text_IO.Put (Integer (O.H.Length), Base => 16);
      New_Line;
      Put_Line ("   Kind : " & O.H.Kind'Img & "   Flags : " & O.H.Flags'Img);
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
         Pad_Length : Byte_1;
         PL_S       : Stream_Element_Array (1 .. 1)
                        with Address => Pad_length'Address;

         P          : Header_Priority_Payload;
         S          : Stream_Element_Array (1 .. 6) with Address => P'Address;
      begin
         --  Read pad-length if corresponding flag set

         if (O.H.Flags and Padded_Flag) = Padded_Flag then
            Net.Buffered.Read (Sock, PL_S);
         end if;

         --  Read header priority patload if corresponding flag set

         if (O.H.Flags and Priority_Flag) = Priority_Flag then
            Net.Buffered.Read (Sock, S);
         end if;

         Net.Buffered.Read (Sock, S);
         Dump ("wu", S);
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
         Put_Line ("@@@ Frame: " & K'Img);
         Net.Buffered.Read (Sock, S);
         Dump (O);
         Dump_Payload (Sock, O);
      end loop;
   end Read;

end AWS.HTTP2.Frame;
