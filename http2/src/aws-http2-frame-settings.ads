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

with Ada.Finalization;
with Ada.Streams;

with System;

with AWS.Net;
with AWS.Utils;

package AWS.HTTP2.Frame.Settings is

   use Ada;

   type Object is new Frame.Object with private;

   type Settings_Kind is
     (HEADER_TABLE_SIZE,
      ENABLE_PUSH,
      MAX_CONCURRENT_STREAMS,
      INITIAL_WINDOW_SIZE,
      MAX_FRAME_SIZE,
      MAX_HEADER_LIST_SIZE);

   type Payload is record
      Id    : Settings_Kind;
      Value : Byte_4;
   end record;

   type Set is array (Stream_Element_Offset range <>) of Payload;

   Empty_Set : constant Set;

   function Read
     (Sock   : Net.Socket_Type'Class;
      Header : Frame.Object) return Object;

   function Ack return Object;

   function Create (Settings : Set) return Object;

   procedure Send_Payload (Sock : Net.Socket_Type'Class; O : Object);

   procedure Dump (O : Object);

private

   for Settings_Kind use (HEADER_TABLE_SIZE      => 16#1#,
                          ENABLE_PUSH            => 16#2#,
                          MAX_CONCURRENT_STREAMS => 16#3#,
                          INITIAL_WINDOW_SIZE    => 16#4#,
                          MAX_FRAME_SIZE         => 16#5#,
                          MAX_HEADER_LIST_SIZE   => 16#6#);

   --  RFC-7540 6.5.1
   --
   --  +-------------------------------+
   --  |       Identifier (16)         |
   --  +-------------------------------+-------------------------------+
   --  |                        Value (32)                             |
   --  +---------------------------------------------------------------+

   for Payload'Bit_Order use System.High_Order_First;
   for Payload'Scalar_Storage_Order use System.High_Order_First;
   for Payload use record
      Id    at 0 range 0 .. 15;
      Value at 2 range 0 .. 31;
   end record;

   subtype Array_Index is
     Stream_Element_Offset range 1 .. Stream_Element_Offset'Last;
   type Maximal_Array_Ptr is access Set (Array_Index)
     with Storage_Size => 0;

   Empty_Set : constant Set := Set'(1 .. 0 => <>);

   type Set_Access is access all Set;

   type Payload_View (Flat : Boolean := False) is record
      case Flat is
         when False => P : Maximal_Array_Ptr;
         when True =>  S : Utils.Stream_Element_Array_Access;
      end case;
   end record with Unchecked_Union;

   type Payload_View_Access is access all Payload_View;

   type Object is new Frame.Object with record
      Size : Stream_Element_Count;
      Data : Payload_View;
   end record;

end AWS.HTTP2.Frame.Settings;
