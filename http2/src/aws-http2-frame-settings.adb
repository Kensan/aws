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

with GNAT.IO; use GNAT.IO;

with AWS.Net.Buffered;

package body AWS.HTTP2.Frame.Settings is

   function Ack return Object is
   begin
      return O : Object := Create (Empty_Set) do
         O.Header.H.Flags := Ack_Flag;
         O.Size := 0;
      end return;
   end Ack;

   function Create (Settings : Set) return Object is
      Len : constant Stream_Element_Count :=
              Stream_Element_Count (Settings'Length * Payload'Size / 8);
   begin
      return O : Object do
         O.Header.H.Stream_Id := 0;
         O.Header.H.Length    := Length_Type (Len);
         O.Header.H.Kind      := K_Settings;
         O.Header.H.R         := 0;
         O.Header.H.Flags     := 0;

         O.Size := Stream_Element_Count (Len / (Payload'Size / 8));

         if Settings'Length > 0 then
            O.Data.S := new Stream_Element_Array
                          (1 .. Stream_Element_Offset (Len));
            O.Data.P (1 .. Settings'Length) := Settings;
         end if;
      end return;
   end Create;

   procedure Dump (O : Object) is
   begin
      for K in 1 .. O.Size loop
         Put_Line ("S: " & O.Data.P (K).Id'Img
                   & " = " & O.Data.P (K).Value'Img);
      end loop;
   end Dump;

   function Read
     (Sock   : Net.Socket_Type'Class;
      Header : Frame.Object) return Object
   is
      Len : constant Stream_Element_Count :=
              Stream_Element_Count (Header.Header.H.Length);
   begin
      return O : Object do
         Frame.Object (O) := Header;
         O.Size := Stream_Element_Count (Len / (Payload'Size / 8));

         if Len > 0 then
            O.Data.S := new Stream_Element_Array (1 .. Len);
            Net.Buffered.Read (Sock, O.Data.S.all);
         end if;
      end return;
   end Read;

   procedure Send_Payload (Sock : Net.Socket_Type'Class; O : Object) is
   begin
      Net.Buffered.Write (Sock, O.Data.S.all);
   end Send_Payload;

end AWS.HTTP2.Frame.Settings;
