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

   Connection_Preface : constant Stream_Element_Array :=
                          (16#50#, 16#52#, 16#49#, 16#20#, 16#2a#, 16#20#,
                           16#48#, 16#54#, 16#54#, 16#50#, 16#2f#, 16#32#,
                           16#2e#, 16#30#, 16#0d#, 16#0a#, 16#0d#, 16#0a#,
                           16#53#, 16#4d#, 16#0d#, 16#0a#, 16#0d#, 16#0a#);

   procedure Create (Kind : Kind_Type; Flags : Flags_Type) is null;

   procedure Dump (O : Object) is
   begin
      Put_Line ("Id : " & O.H.Id'Img);
      Put ("   L    : " & O.H.Length'Img);
      Integer_Text_IO.Put (O.H.Length, Base => 16);
      New_Line;
      Put_Line ("   Kind : " & O.H.Kind'Img);
   end Dump;

   procedure Skip_Payload (Sock : Net.Socket_Type'Class; O : Object) is
      S : Stream_Element_Array (1 .. Stream_Element_Offset (O.H.Length));
   begin
      Net.Buffered.Read (Sock, S);
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
         Skip_Payload (Sock, O);
      end loop;
   end Read;

end AWS.HTTP2.Frame;
