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

package body AWS.HTTP2.Stream is

   ------------
   -- Create --
   ------------

   function Create (Identifier : Id) return Object is
   begin
      return Object'(Identifier, Idle, Frame.List.Empty_List, False);
   end Create;

   ----------------------
   -- Is_Message_Ready --
   ----------------------

   function Is_Message_Ready (Self : Object) return Boolean is
   begin
      return Self.Is_Ready;
   end Is_Message_Ready;

   -------------
   -- Message --
   -------------

   function Message (Self : Object) return HTTP2.Message.Object is
      O : HTTP2.Message.Object;
   begin
      return O;
   end Message;

   ----------------
   -- Push_Frame --
   ----------------

   procedure Push_Frame
     (Self  : in out Object;
      Frame : HTTP2.Frame.Object'Class) is
   begin
      Frame.Dump;
      Self.Frames.Append (Frame);


   end Push_Frame;

end AWS.HTTP2.Stream;
