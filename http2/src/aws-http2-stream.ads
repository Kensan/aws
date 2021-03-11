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

with AWS.HTTP2.Frame;
with AWS.HTTP2.Message;
with AWS.HTTP2.Frame.List;

package AWS.HTTP2.Stream is

   type State_Kind is (Idle, Reserved, Open, Half_Closed, Closed);

   type Object is tagged private;

   subtype Id is Stream_Id;

   function Create (Identifier : Id) return Object
     with Post => Create'Result.State = Idle;

   function State (Self : Object) return State_Kind;

   function Identifier (Self : Object) return Id;

   procedure Push_Frame
     (Self  : in out Object;
      Frame : HTTP2.Frame.Object'Class);

   function Is_Message_Ready (Self : Object) return Boolean
     with Post => (if Is_Message_Ready'Result then Self.State >= Open);

   function Message (Self : Object) return HTTP2.Message.Object
     with Pre => Self.Is_Message_Ready;

private

   type Object is tagged record
      Id        : Stream.Id;
      State     : State_Kind;
      Frames    : Frame.List.Object;
      Is_Ready  : Boolean := False;
   end record;

   function State (Self : Object) return State_Kind is (Self.State);

   function Identifier (Self : Object) return Id is (Self.Id);

end AWS.HTTP2.Stream;
