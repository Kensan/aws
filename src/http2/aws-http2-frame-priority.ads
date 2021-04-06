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

with System;

package AWS.HTTP2.Frame.Priority is

   type Object is new Frame.Object with private;

   type Payload is record
      E                 : Bit_1;
      Stream_Dependency : HTTP2.Stream_Id;
      Weight            : Byte_1;
   end record;

   function Create
     (Stream_Id         : HTTP2.Stream_Id;
      Stream_Dependency : HTTP2.Stream_Id;
      Weight            : Byte_1) return Object
     with Pre => Stream_Id /= Stream_Dependency;
   --  Create a PRIORITY frame (stream id is always 0)

   function Read
     (Sock   : Net.Socket_Type'Class;
      Header : Frame.Object) return Object
     with Pre => Header.Is_Defined;
   --  Read PRIORITY frame from sock

   overriding procedure Send_Payload
     (Self : Object; Sock : Net.Socket_Type'Class);
   --  Send the priority frame payload

   overriding procedure Dump_Payload (Self : Object);

private

   --  RFC-7540 6.3
   --
   --  +-+-------------------------------------------------------------+
   --  |E|                  Stream Dependency (31)                     |
   --  +-+-------------+-----------------------------------------------+
   --  |   Weight (8)  |
   --  +-+-------------+

   for Payload'Bit_Order use System.High_Order_First;
   for Payload'Scalar_Storage_Order use System.High_Order_First;
   for Payload use record
      E                 at 0 range 31 .. 31;
      Stream_Dependency at 0 range  0 .. 30;
      Weight            at 4 range  0 ..  7;
   end record;

   type Payload_View (Flat : Boolean := False) is record
      case Flat is
         when False => P : Payload;
         when True =>  S : Stream_Element_Array (1 .. Payload'Size / 8);
      end case;
   end record with Unchecked_Union;

   type Object is new Frame.Object with record
      Data : Payload_View;
   end record;

end AWS.HTTP2.Frame.Priority;
