------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                         Copyright (C) 2002-2003                          --
--                                ACT-Europe                                --
--                                                                          --
--  Authors: Dmitriy Anisimkov - Pascal Obry                                --
--                                                                          --
--  This library is free software; you can redistribute it and/or modify    --
--  it under the terms of the GNU General Public License as published by    --
--  the Free Software Foundation; either version 2 of the License, or (at   --
--  your option) any later version.                                         --
--                                                                          --
--  This library is distributed in the hope that it will be useful, but     --
--  WITHOUT ANY WARRANTY; without even the implied warranty of              --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU       --
--  General Public License for more details.                                --
--                                                                          --
--  You should have received a copy of the GNU General Public License       --
--  along with this library; if not, write to the Free Software Foundation, --
--  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.          --
--                                                                          --
--  As a special exception, if other files instantiate generics from this   --
--  unit, or you link this unit with other files to produce an executable,  --
--  this  unit  does not  by itself cause  the resulting executable to be   --
--  covered by the GNU General Public License. This exception does not      --
--  however invalidate any other reasons why the executable file  might be  --
--  covered by the  GNU Public License.                                     --
------------------------------------------------------------------------------

--  $Id$

with Ada.Streams;

with ZLib;

with AWS.Utils;

package AWS.Resources.Embedded is

   use Ada;

   type Buffer_Access is access constant Streams.Stream_Element_Array;

   type Data_Mode is (Compressed, Uncompressed);

   procedure Open
     (File :    out File_Type;
      Name : in     String;
      Form : in     String    := "");
   --  Open resource from registered data

   procedure Create
     (File   :    out File_Type;
      Buffer : in     Buffer_Access;
      Mode   : in     Data_Mode     := Uncompressed);
   --  Create the resource directly from memory data

   function Is_Regular_File (Name : in String) return Boolean;

   function File_Size
     (Name : in String)
      return Ada.Streams.Stream_Element_Offset;

   function File_Timestamp (Name : in String) return Ada.Calendar.Time;

   procedure Register
     (Name      : in String;
      Content   : in Buffer_Access;
      File_Time : in Calendar.Time;
      Mode      : in Data_Mode     := Uncompressed);
   --  Register a new file named Name into the embedded resources. The file
   --  content is pointed to by Content, the File_Time must be the last
   --  modification time stamp for the file.

   function Exists (Name : in String) return Boolean;
   pragma Inline (Exists);
   --  Returns True if file named Name has been registered (i.e. it is an
   --  in-memory file).

private

   --  Uncompressed embedded resources

   type File_Tagged is new Resources.File_Tagged with record
      Buffer : Buffer_Access;
      K      : Streams.Stream_Element_Offset;
   end record;

   function End_Of_File (Resource : in File_Tagged) return Boolean;

   procedure Read
     (Resource : in out File_Tagged;
      Buffer   :    out Stream_Element_Array;
      Last     :    out Stream_Element_Offset);

   function Size (Resource : in File_Tagged) return Stream_Element_Offset;

   procedure Close (Resource : in out File_Tagged);

   procedure Reset (Resource : in out File_Tagged);

   --  Compressed embedded resources

   type Z_File_Tagged is new File_Tagged with record
      --  Buffer to uncompress on the fly compressed data if the client does
      --  not support compressed data.
      U_Buffer           : Utils.Stream_Element_Array_Access;
      R_First, R_Last    : Stream_Element_Offset;
      --  Filter used to uncompress data
      U_Filter           : ZLib.Filter_Type;
   end record;

   procedure Read
     (Resource : in out Z_File_Tagged;
      Buffer   :    out Stream_Element_Array;
      Last     :    out Stream_Element_Offset);

   procedure Close (Resource : in out Z_File_Tagged);

   procedure Support_Compressed
     (Resource : in out Z_File_Tagged;
      State    : in     Boolean);

end AWS.Resources.Embedded;
