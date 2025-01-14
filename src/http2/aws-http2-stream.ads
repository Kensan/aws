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

with Ada.Streams;

with AWS.Net;
with AWS.Server.Context;

with AWS.Headers;
with AWS.HTTP2.Frame;
with AWS.HTTP2.Frame.List;
with AWS.HTTP2.Frame.Priority;
with AWS.Response;
with AWS.Status;

package AWS.HTTP2.Stream is

   use Ada.Streams;

   type State_Kind is (Idle, Reserved_Local, Reserved_Remote, Open,
                       Half_Closed_Local, Half_Closed_Remote, Closed);
   --  RFC 7540 5.1 Stream States

   type Object is tagged private;

   Undefined : constant Object;

   function Is_Defined (Self : Object) return Boolean;

   function "<" (Left, Right : Object) return Boolean;

   subtype Id is Stream_Id;

   function Create
     (Sock        : not null Net.Socket_Access;
      Identifier  : Id;
      Window_Size : Natural;
      Weight      : Byte_1 := Frame.Priority.Default_Weight) return Object
     with Post => Create'Result.State = Idle
                  and then Create'Result.Is_Defined;
   --  Create a stream object for the given connection

   function State (Self : Object) return State_Kind
     with Pre => Self.Is_Defined;
   --  Return stream current state

   function Identifier (Self : Object) return Id
     with Pre => Self.Is_Defined;
   --  Returns stream identifier

   procedure Send_Frame
     (Self  : in out Object;
      Frame : HTTP2.Frame.Object'Class)
     with Pre => Self.Is_Defined and then Frame.Is_Defined;
   --  Send a frame to the stream. Possibly change the state of the stream or
   --  raise an exception if the flow is not correct.

   procedure Received_Frame
     (Self  : in out Object;
      Ctx   : in out Server.Context.Object;
      Frame : HTTP2.Frame.Object'Class;
      Error : out Error_Codes)
     with Pre => Self.Is_Defined and then Frame.Is_Defined;
   --  Record a stream being received for this stream. Possibly change the
   --  state of the stream or raise an exception if the flow is not correct.

   function Is_Message_Ready (Self : Object) return Boolean
     with Post => (if Is_Message_Ready'Result then Self.State >= Open);
   --  Returns True if a message is ready on this stream

   procedure Append_Body (Self : Object; Status : in out AWS.Status.Data)
     with Pre => Self.Is_Defined and then Self.Is_Message_Ready;
   --  Append status body from stream data frames

   function Headers (Self : Object) return AWS.Headers.List
     with Pre => Self.Is_Defined and then Self.Is_Message_Ready;
   --  Returns headers taken from stream

   function Priority (Self : Object) return Byte_1
     with Pre => Self.Is_Defined;
   --  Returns the assigned stream priority

   function Flow_Control_Window (Self : Object) return Integer
     with Pre => Self.Is_Defined;
   --  Return the current window size for this frame (set by the routine below)

   procedure Update_Flow_Control_Window
     (Self      : in out Object;
      Increment : Integer)
     with Pre => Self.Is_Defined;
   --  Set the window size for this frame (set via the Window_Update frame)

   function Bytes_Sent (Self : Object) return Stream_Element_Count;
   --  Number of payload bytes send over this stream

   function Status (Self : Object) return not null access AWS.Status.Data;

   function Response (Self : Object) return not null access AWS.Response.Data;

private

   subtype Content_Length_Type is
     Stream_Element_Offset range -1 .. Stream_Element_Offset'Last;

   Undefined_Length : constant Content_Length_Type := -1;

   type Object is tagged record
      Sock                : AWS.Net.Socket_Access;
      Id                  : Stream.Id            := 0;
      State               : State_Kind           := Idle;
      H_Frames            : Frame.List.Object; -- Header frames
      D_Frames            : Frame.List.Object; -- Data frames
      Headers             : AWS.Headers.List;
      Status              : aliased AWS.Status.Data;
      Response            : aliased AWS.Response.Data;
      Is_Ready            : Boolean              := False;
      Header_Found        : Boolean              := False;
      Flow_Send_Window    : Integer;
      Flow_Receive_Window : Integer;
      Bytes_Sent          : Stream_Element_Count := 0;
      Weight              : Byte_1;
      Stream_Dependency   : HTTP2.Stream_Id;
      End_Stream          : Boolean              := False;
      Content_Length      : Content_Length_Type  := Undefined_Length;
      Bytes_Received      : Content_Length_Type  := 0;
   end record;

   function "<" (Left, Right : Object) return Boolean is (Left.Id < Right.Id);

   Undefined : constant Object :=
                 (null, 0, Idle, Frame.List.Empty_List, Frame.List.Empty_List,
                  Headers             => AWS.Headers.Empty_List,
                  Status              => <>,
                  Response            => <>,
                  Flow_Send_Window    => 0,
                  Flow_Receive_Window => 0,
                  Weight              => 0,
                  Stream_Dependency   => 0,
                  others              => <>);

   function State (Self : Object) return State_Kind is (Self.State);

   function Identifier (Self : Object) return Id is (Self.Id);

   function Flow_Control_Window (Self : Object) return Integer is
     (Self.Flow_Send_Window);

   function Bytes_Sent (Self : Object) return Stream_Element_Count is
     (Self.Bytes_Sent);

   function Priority (Self : Object) return Byte_1 is
     (Self.Weight);

   function Headers (Self : Object) return AWS.Headers.List is
     (Self.Headers);

   function Status (Self : Object) return not null access AWS.Status.Data is
     (Self.Status'Unrestricted_Access);

   function Response (Self : Object) return not null access AWS.Response.Data
   is (Self.Response'Unrestricted_Access);

   function Is_Defined (Self : Object) return Boolean is (Self /= Undefined);

end AWS.HTTP2.Stream;
