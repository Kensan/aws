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
with Ada.Text_IO;

with System;

with AWS.HPACK.Table;
with AWS.HPACK.Huffman;
with AWS.Net.Buffered;
with AWS.Translator;

package body AWS.HPACK is

   use Ada.Streams;
   use Ada.Text_IO;

   T : AWS.HPACK.Table.Object;

   subtype Bit1 is Stream_Element range 0 .. 1;
   subtype Bit2 is Stream_Element range 0 .. 3;
   subtype Bit4 is Stream_Element range 0 .. 15;
   subtype Bit8 is Stream_Element range 0 .. 255;

   type Group_Size is (G1, G2, G4);

   type RFC_Byte (Group : Group_Size := Group_Size'First) is record
      case Group is
         when G1 =>
            B0 : Bit1;
            B1 : Bit1;
            B2 : Bit1;
            B3 : Bit1;
            B4 : Bit1;
            B5 : Bit1;
            B6 : Bit1;
            B7 : Bit1;

         when G2 =>
            B20 : Bit2;
            B21 : Bit2;
            B22 : Bit2;
            B23 : Bit2;

         when G4 =>
            B40 : Bit4;
            B41 : Bit4;
      end case;
   end record with Size => 8, Unchecked_Union;

   for RFC_Byte use record
      B0 at 0 range 7 .. 7;
      B1 at 0 range 6 .. 6;
      B2 at 0 range 5 .. 5;
      B3 at 0 range 4 .. 4;
      B4 at 0 range 3 .. 3;
      B5 at 0 range 2 .. 2;
      B6 at 0 range 1 .. 1;
      B7 at 0 range 0 .. 0;
      --
      B20 at 0 range 6 .. 7;
      B21 at 0 range 4 .. 5;
      B22 at 0 range 2 .. 3;
      B23 at 0 range 0 .. 1;
      --
      B40 at 0 range 4 .. 7;
      B41 at 0 range 0 .. 3;
   end record;

   B_II          : constant Bit2 := 2#01#;
   --  Incremental Indexing

   B_II_New_Name : constant Bit8 := 2#0100_0000#;
   --  Incremental Indexing new name

   B_No_Indexing : constant Bit4 := 0;
   --  No Indexing

   Zero          : constant Bit8 := 0;

   function Get_Indexed_Name (Idx : Stream_Element) return String;
   procedure Get_Indexed_Name_Value (Idx : Stream_Element);

   procedure Get_Headers
     (Sock   : Net.Socket_Type'Class;
      Length : Stream_ELement_Offset)
   is

      Byte : Bit8;
      Bit  : RFC_Byte (G1) with Address => Byte'Address;
      BG2  : RFC_Byte (G2) with Address => Byte'Address;
      BG4  : RFC_Byte (G4) with Address => Byte'Address;

      Len  : Stream_Element_Offset := 0;

      function Get_String_Literal return String;

      function Get_String_Literal return String is
      begin
         --  String Literal (RFC-7541 / 5.2)
         --
         --     0   1   2   3   4   5   6   7
         --   +---+---+---+---+---+---+---+---+
         --   | H |    String Length (7+)     |
         --   +---+---------------------------+
         --   |  String Data (Length octets)  |
         --   +-------------------------------+

         Byte := Net.Buffered.Get_Byte (Sock);
         Len := Len + 1;

         declare
            Length : constant Stream_Element := Byte and 2#0111_1111#;
            Str    : Stream_Element_Array
                       (1 .. Stream_Element_Offset (Length));
         begin
            Put_Line ("Read: " & Length'Img);

            Net.Buffered.Read (Sock, Str);
            Len := Len + Stream_Element_Offset (Length);

            if Bit.B0 = 1 then
               --  Huffman encode

               return Huffman.Decode (Str);

            else
               --  Plain literal

               return Translator.To_String (Str);
            end if;
         end;
      end Get_String_Literal;

      Idx : Stream_Element;

   begin
      loop
         Byte := Net.Buffered.Get_Byte (Sock);
         Len := Len + 1;

--         Put_Line ("H " & Byte'Img & "    RFC " & Bit.B0'Img & Bit.B1'Img);
--         Put_Line ("      RFC B2: " & BG2.B20'Img);
--         Put_Line ("      RFC B4: " & BG4.B40'Img);

         if Bit.B0 = 1 then
            --  Indexed header field (RFC-7541 / 6.1)
            --
            --    0   1   2   3   4   5   6   7
            --  +---+---+---+---+---+---+---+---+
            --  | 1 |        Index (7+)         |
            --  +---+---------------------------+

            Put_Line ("B0 = 1 (indexed)");

            Idx := Byte and 2#0111_1111#;

            Put_Line ("   idx = " & Idx'Img);

            Get_Indexed_Name_Value (Idx);

         else
            if Byte = B_II_New_Name then
               --  Incremental indexing - New Name (RFC-7541 / 6.2.1)
               --
               --    0   1   2   3   4   5   6   7
               --  +---+---+---+---+---+---+---+---+
               --  | 0 | 1 |           0           |
               --  +---+---+-----------------------+

               Idx := 0;

               Put_Line ("B_II_New_Name");

               --  Get the name/value

               declare
                  Name  : constant String := Get_String_Literal;
                  Value : constant String := Get_String_Literal;
               begin
                  Put_Line (Name & " - " & Value);

                  T.Insert (Name, Value);
               end;

            elsif BG2.B20 = B_II then
               --  Incremental indexing - Indexed name (RFC-7541 / 6.2.1)
               --
               --    0   1   2   3   4   5   6   7
               --  +---+---+---+---+---+---+---+---+
               --  | 0 | 1 |      Index (6+)       |
               --  +---+---+-----------------------+

               Put_Line ("B_II");

               Idx := Byte and 2#0011_1111#;

               declare
                  Name  : constant String := Get_Indexed_Name (Idx);
               begin
                  Put_Line ("******* " & Name);

               declare
                  Value : constant String := Get_String_Literal;
               begin
                  Put_Line (Name & " - " & Value);

                  T.Insert (Name, Value);
               end;
               end;

            elsif Byte = Zero then
               --  No indexing - New Name (RFC-7541 / 6.2.2)
               --
               --    0   1   2   3   4   5   6   7
               --  +---+---+---+---+---+---+---+---+
               --  | 0 | 0 | 0 | 0 |       0       |
               --  +---+---+-----------------------+

               Idx := 0;

               Put_Line ("B_NI_New_Name");

               --  Get the name/value

               declare
                  Name  : constant String := Get_String_Literal;
                  Value : constant String := Get_String_Literal;
               begin
                  Put_Line (Name & " - " & Value);
               end;

            elsif BG4.B40 = B_No_Indexing then
               --  No indexing - Indexed Name (RFC-7541 / 6.2.2)
               --
               --     0   1   2   3   4   5   6   7
               --   +---+---+---+---+---+---+---+---+
               --   | 0 | 0 | 0 | 0 |  Index (4+)   |
               --   +---+---+-----------------------+

               Idx := Byte and 2#0000_1111#;

               Put_Line ("B_No_Indexing " & Byte'Img & Idx'Img);

               declare
                  Name  : constant String := Get_Indexed_Name (Idx);
               begin
                  Put_Line ("Name only: " & Name);
               declare
                  Value : constant String := Get_String_Literal;
               begin
                  Put_Line (Name & " - " & Value);
               end;
               end;

            else
               Put_Line ("================ HPACK NOT YET SUPPORTED");
            end if;

            --  ??? 6.2.3 (not coverred for now)
         end if;

         Table.Dump (T);

         exit when Len = Length;
      end loop;
   end Get_Headers;

   function Get_Indexed_Name (Idx : Stream_Element) return String is
   begin
      return HPACK.Table.Get_Name (T, Positive (Idx));
   end Get_Indexed_Name;

   procedure Get_Indexed_Name_Value (Idx : Stream_Element) is
      I : constant HPACK.Table.Name_Value :=
            HPACK.Table.Get_Name_Value (T, Positive (Idx));
   begin
      Put_Line (I.Name & " " & I.Value);
   end Get_Indexed_Name_Value;

   function Encode return Stream_Element_Array is

      Res : Stream_Element_Array (1 .. 1000);
      I   : Stream_Element_Offset := 0;

      procedure Append (E : Stream_Element) is
      begin
         I := I + 1;
         Res (I) := E;
      end Append;

      procedure Send (Str : String) is
         H : Bit8;
         S : Stream_Element_Array (1 .. 1) with Address => H'Address;
      begin
         H := Str'Length;
         Append (S (1));

         for K in Str'Range loop
            Append (Stream_Element (Character'Pos (Str (K))));
         end loop;
      end Send;

      CRLF : constant String := String'(ASCII.CR, ASCII.LF);

      H : Bit8;
      S : Stream_Element_Array (1 .. 1) with Address => H'Address;

   begin
      H := Stream_Element (128 + Table.Get_Name_Index (T, ":status=200"));
      Append (S (1));

      H := Stream_Element (64 + Table.Get_Name_Index (T, "content-type"));
      Append (S (1));
      Send ("text/html"); --  & CRLF);

      H := Stream_Element (64 + Table.Get_Name_Index (T, "server"));
      Append (S (1));
      Send ("aws");

--      H := Stream_Element (64 + Table.Get_Name_Index (T, "content-length"));
--      Append (S (1));
--      Send ("21");

      Put_Line ("I=" & I'Img);
--      H := 0;
--      Append (S (1));
--      Send (CRLF);
--      Send ("");

      return Res (1 .. I);
   end Encode;

begin
   AWS.HPACK.Table.Init (T);
end AWS.HPACK;
