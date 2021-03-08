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

pragma Warnings (Off);

separate (AWS.Server)

procedure Protocol_Handler_V2 (LA : in out Line_Attribute_Record) is

   --  A task reading the frames
   --  A set of stream object to keep current status
   --

   use Ada.Text_IO;
   use AWS.Server.HTTP_Utils;

   Case_Sensitive_Parameters : constant Boolean :=
                                 CNF.Case_Sensitive_Parameters
                                   (LA.Server.Properties);

   Sock_Ptr     : Socket_Access;

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

begin
   --  This new connection has been initialized because some data are being
   --  sent. We are by default using HTTP/1.1 persistent connection. We will
   --  exit this loop only if the client request so or if we time-out on
   --  waiting for a request.

   LA.Log_Data := AWS.Log.Empty_Fields_Table;

   For_Every_Request : loop
      declare
         use Ada.Streams;
         use type Response.Data_Mode;

         Expectation_Failed : exception;

         Error_Answer : Response.Data;
         Back_OK      : Boolean;
         First_Line   : Boolean := True;
         Switch       : constant array (Boolean) of
                          not null access function
                            (Socket : Net.Socket_Type'Class;
                             Events : Net.Wait_Event_Set) return Net.Event_Set
                          := (True  => Net.Wait'Access,
                              False => Net.Check'Access);

         function Send_Error_Answer return Boolean;
         --  Send Error_Answer to the client, returns False if an(other)
         --  error occurs while trying to send the error answer.

         -----------------------
         -- Send_Error_Answer --
         -----------------------

         function Send_Error_Answer return Boolean is
         begin
            Send
              (Error_Answer, LA.Server.all, LA.Line, LA.Stat, Socket_Taken,
               Will_Close);
            return True;

         exception
            when Net.Socket_Error =>
               Will_Close := True;
               return False;

            when E : others =>
               AWS.Log.Write
                 (LA.Server.Error_Log,
                  LA.Stat,
                  Utils.CRLF_2_Spaces
                    (Ada.Exceptions.Exception_Information (E)));
               return False;
         end Send_Error_Answer;

      begin
         Put_Line ("Switched in v2 protocol...");
         delay 1.0;

         Response.Set.Mode (Error_Answer, Response.No_Data);

         LA.Server.Slots.Mark_Phase (LA.Line, Client_Header);

         if Sock_Ptr = null then
            --  First arrived. We do not need to wait for fast comming next
            --  keep alive request.

            Sock_Ptr := LA.Server.Slots.Get (Index => LA.Line).Sock;
         end if;

         --  Let's try to create a hand made HTTP/2 request/response

      end;
   end loop For_Every_Request;

   --  Release memory for local objects

   AWS.Status.Set.Free (LA.Stat);
end Protocol_Handler_V2;
