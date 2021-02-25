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

with AWS.Translator;

package body AWS.HTTP2.Frame.Data is

   ------------
   -- Create --
   ------------

   function Create (Content : String) return Object is
      Len : constant Stream_Element_Count :=
              Stream_Element_Count (Content'Length);
   begin
      return O : Object do
         O.Header.H.Stream_Id := 1;
         O.Header.H.Length    := Length_Type (Len);
         O.Header.H.Kind      := K_Data;
         O.Header.H.R         := 0;
         O.Header.H.Flags     := End_Stream_Flag;

         if Content'Length > 0 then
            O.Data.S := Translator.To_Stream_Element_Array (Content);
         end if;
      end return;
   end Create;

   ----------
   -- Dump --
   ----------

   procedure Dump (O : Object) is
   begin
      null;
   end Dump;

   ----------
   -- Read --
   ----------

   function Read
     (Sock   : Net.Socket_Type'Class;
      Header : Frame.Object) return Object
   is
      Len : constant Stream_Element_Count :=
              Stream_Element_Count (Header.Header.H.Length);
   begin
      return O : Object do
         Frame.Object (O) := Header;

         if Len > 0 then
            O.Data.S := new Stream_Element_Array (1 .. Len);
            Net.Buffered.Read (Sock, O.Data.S.all);
         end if;
      end return;
   end Read;

   ------------------
   -- Send_Payload --
   ------------------

   overriding procedure Send_Payload
     (Sock : Net.Socket_Type'Class; O : Object) is
   begin
      Net.Buffered.Write (Sock, O.Data.S.all);
   end Send_Payload;

end AWS.HTTP2.Frame.Data;
