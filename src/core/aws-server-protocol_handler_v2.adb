------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                       Copyright (C) 2021, AdaCore                        --
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

--  This procedure is responsible of handling the HTTP protocol. Every
--  responses and incoming requests are parsed/formated here.

pragma Warnings (Off);
pragma Style_Checks (Off);

with Ada.Exceptions;
with Ada.Streams;
with Ada.Text_IO;

with AWS.Log;
with AWS.Messages;
with AWS.Net.Buffered;
with AWS.Parameters;
with AWS.Response.Set;
with AWS.Server.HTTP_Utils;
with AWS.Server.Status;
with AWS.Status.Set;
with AWS.Utils;

with AWS.HTTP2.Frame.List;
with AWS.HTTP2.Message;
with AWS.HTTP2.Stream;
with AWS.HTTP2.Frame.Settings;
with AWS.HTTP2.Frame.Window_Update;

pragma Warnings (Off);

separate (AWS.Server)

procedure Protocol_Handler_V2 (LA : in out Line_Attribute_Record) is

   --  A task reading the frames
   --  A set of stream object to keep current status
   --

   use Ada.Text_IO;
   use Ada.Streams;

   use AWS.Server.HTTP_Utils;

   use type HTTP2.Stream_Id;
   use type HTTP2.Frame.Length_Type;
   use all type HTTP2.Frame.Kind_Type;

   CTRL_Stream : HTTP2.Stream.Object;

   Answers : HTTP2.Frame.List.Object;

   procedure Handle_Control_Frame (Frame : HTTP2.Frame.Object'Class);

   procedure Handle_Message (Stream : HTTP2.Stream.Object);

   --------------------------
   -- Handle_Control_Frame --
   --------------------------

   procedure Handle_Control_Frame (Frame : HTTP2.Frame.Object'Class) is

      procedure Handle (Frame : HTTP2.Frame.Settings.Object);

      procedure Handle (Frame : HTTP2.Frame.Window_Update.Object);

      ------------
      -- Handle --
      ------------

      procedure Handle (Frame : HTTP2.Frame.Settings.Object) is
      begin
         Put_Line ("@SETTINGS");

         declare
            SP : HTTP2.Frame.Settings.Payload;
         begin
            SP.Id := HTTP2.Frame.Settings.MAX_CONCURRENT_STREAMS;
            SP.Value := 326;

            Answers.Append
              (HTTP2.Frame.Settings.Create
                 (HTTP2.Frame.Settings.Set'(1 => SP)));
         end;
      end Handle;

      procedure Handle (Frame : HTTP2.Frame.Window_Update.Object) is
      begin
         Put_Line ("@WINDOW_UPDTAE");
      end Handle;

   begin
      Put_Line ("CTRL: " & Frame.Kind'Img);
      Frame.Dump;

      if Frame.Kind = K_Settings then
         Handle (HTTP2.Frame.Settings.Object (Frame));

      elsif Frame.Kind = K_Window_Update then
         if Frame.Length = 4 then
            Handle (HTTP2.Frame.Window_Update.Object (Frame));
         else
            --  ??? push an error frame into answer with FRAME_SIZE_ERROR
            null;
         end if;
      end if;

      --  CTRL_Stream.Push_Frame (Frame);
   end Handle_Control_Frame;

   --------------------
   -- Handle_Message --
   --------------------

   procedure Handle_Message (Stream : HTTP2.Stream.Object) is
      M : constant HTTP2.Message.Object := Stream.Message;
   begin
      null;
   end Handle_Message;

   Sock_Ptr     : Socket_Access :=
                    LA.Server.Slots.Get (Index => LA.Line).Sock;

   Socket_Taken : Boolean := False;
   --  Set to True if a socket has been reserved for a push session

   Will_Close   : Boolean := True;
   --  Will_Close is set to true when the connection will be closed by the
   --  server. It means that the server is about to send the latest message
   --  to the client using this socket. The value will be changed by
   --  Set_Close_Status.

   Keep_Alive_Limit : constant Natural :=
                        CNF.Free_Slots_Keep_Alive_Limit (LA.Server.Properties);

   Free_Slots   : Natural;

   Extended_Log : constant Boolean :=
                    CNF.Log_Extended_Fields_Length (LA.Server.Properties) > 0;

   Multislots   : constant Boolean :=
                    CNF.Max_Connection (LA.Server.Properties) > 1;

   S : array (HTTP2.Stream_Id range 1 .. 100) of HTTP2.Stream.Object;

begin
   --  This new connection has been initialized because some data are being
   --  sent. We are by default using HTTP/1.1 persistent connection. We will
   --  exit this loop only if the client request so or if we time-out on
   --  waiting for a request.

   LA.Log_Data := AWS.Log.Empty_Fields_Table;

   --  When swicthing protocol the first bytes are a preface

   declare
      Connection_Preface : constant Stream_Element_Array :=
                             (16#50#, 16#52#, 16#49#, 16#20#, 16#2a#, 16#20#,
                              16#48#, 16#54#, 16#54#, 16#50#, 16#2f#, 16#32#,
                              16#2e#, 16#30#, 16#0d#, 16#0a#, 16#0d#, 16#0a#,
                              16#53#, 16#4d#, 16#0d#, 16#0a#, 16#0d#, 16#0a#);
      Preface            : Stream_Element_Array (1 .. 24);
   begin
      Net.Buffered.Read (Sock_Ptr.all, Preface);

      if Preface = Connection_Preface then
         Put_Line ("OK connection preface");
         Put_Line ("Switched in v2 protocol...");
      else
         raise Constraint_Error with "connection preface not found";
      end if;
   end;

   For_Every_Frame : loop
      --  Get frame

      declare
         use Ada.Streams;
         use type Response.Data_Mode;

         Expectation_Failed : exception;

         Error_Answer : Response.Data;
         Back_OK      : Boolean;

         Frame        : HTTP2.Frame.Object'Class :=
                          HTTP2.Frame.Read (Sock_Ptr.all);
         Stream_Id    : constant HTTP2.Stream_Id := Frame.Stream_Id;
      begin
         delay 1.0;

         if Stream_Id = 0 then
            Handle_Control_Frame (Frame);

         else
            S (Stream_Id).Push_Frame (Frame);

            if S (Stream_Id).Is_Message_Ready then
               Handle_Message (S (Stream_Id));
            end if;
         end if;
      end;

      --  Send back frames if any waiting

      while not Answers.Is_Empty loop
         HTTP2.Frame.Send (Sock_Ptr.all, Answers.First_Element);
         Answers.Delete_First;
      end loop;
   end loop For_Every_Frame;

   --  Release memory for local objects

   AWS.Status.Set.Free (LA.Stat);
end Protocol_Handler_V2;
