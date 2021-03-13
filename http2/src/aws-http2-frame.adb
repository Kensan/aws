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
with AWS.HTTP2.Frame.RST_Stream;

package body AWS.HTTP2.Frame is

   use Ada;
   use Ada.Streams;
   use System;

   procedure Dump (O : Object'Class) is
   begin
      Put ("FRAME: Id:" & Integer (O.Header.H.Stream_Id)'Img);
      Put ("  L:" & Integer (O.Header.H.Length)'Img);
      Integer_Text_IO.Put (Integer (O.Header.H.Length), Width => 8, Base => 16);
      Put_Line ("  Kind: " & O.Header.H.Kind'Img & "   Flags:" & O.Header.H.Flags'Img);
   end Dump;

   procedure Dump_Payload (Sock : Net.Socket_Type'Class; O : Object'Class) is
      use Utils;
   begin
      if O.Header.H.Kind = K_Settings then
         Settings.Dump (Settings.Object (O));

      elsif O.Header.H.Kind = K_Window_Update then
         Window_Update.Dump (Window_Update.Object (O));

      elsif O.Header.H.Kind = K_Headers then
         Headers.Dump (Headers.Object (O));

      elsif O.Header.H.Kind = K_RST_Stream then
         RST_Stream.Dump (RST_Stream.Object (O));

      else
         Put_Line ("=> YET unsupported frame, skip payload");
         declare
            S : Stream_Element_Array (1 .. Stream_Element_Offset (O.Header.H.Length));
         begin
            Net.Buffered.Read (Sock, S);
         end;
      end if;
   end Dump_Payload;

   ----------
   -- Read --
   ----------

   procedure Go (Sock : Net.Socket_Type'Class) is
   begin
      --  Get Frames

      for k in 1 .. 3 loop
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
      --  ?? we can do this case with directly in Ada with dispatching.
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
         when K_RST_Stream =>
            return Frame.RST_Stream.Read (Sock, H);
         when Others =>
            return H;
      end case;
   end Read;

   procedure Send (Sock : Net.Socket_Type'Class; O : Object'Class) is
      use type Utils.Stream_Element_Array_Access;
   begin
      Net.Buffered.Write (Sock, O.Header.S);

      if O.Header.H.Length > 0 then
         Send_Payload (Sock, Object'Class (O));
      end if;

      Net.Buffered.Flush (Sock);
   end Send;

end AWS.HTTP2.Frame;
