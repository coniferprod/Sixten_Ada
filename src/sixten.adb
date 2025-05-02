with Ada.Streams.Stream_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Directories; use Ada.Directories;

package body Sixten is

   -- Helper function to increment an integer value by one.
   function Inc (I : Integer) return Integer is (I + 1)
      with Inline;

   -- Helper function to decrement an integer value by one.
   function Dec (I : Integer) return Integer is (I - 1)
      with Inline;

   function To_Byte_Array (V : Byte_Vector) return Byte_Array is
      Temp_Arr: Byte_Array (0 .. Natural (V.Length) - 1);
   begin
      for I in Temp_Arr'Range loop
         Temp_Arr (I) := V.Element (I);
      end loop;
      return Temp_Arr;
   end To_Byte_Array;

   function To_Byte_Vector (Data : Byte_Array) return Byte_Vector is
      BV : Byte_Vector;
   begin
      for I in Data'Range loop
         BV.Append (Byte (Data (I)));
      end loop;
      return BV;
   end To_Byte_Vector;

   procedure Write_File (Name : String; Contents : Byte_Vector) is
      use Byte_IO;

      F : File_Type;
   begin
      Create (F, Out_File, Name);

      for B of Contents loop
         Write (F, B);
      end loop;

      Close (F);
   end Write_File;

   procedure Read_File (Name : String; Contents : out Byte_Array) is
      package SIO renames Ada.Streams.Stream_IO;

      Input_File   : SIO.File_Type;
      Input_Stream : SIO.Stream_Access;
      Index        : File_Size := 0;
      B            : Byte;
   begin
      SIO.Open (Input_File, SIO.In_File, Name);

      Input_Stream := SIO.Stream (Input_File);
      while not SIO.End_Of_File (Input_File) loop
         Byte'Read (Input_Stream, B);
         Contents (Integer (Index)) := B;
         Index            := Index + 1;
      end loop;

      SIO.Close (Input_File);
   end Read_File;

   function Read_All_Bytes (File_Name : String) return Byte_Vector is
      package SIO renames Ada.Streams.Stream_IO;

      Buffer : Byte_Vector;
      Input_File   : SIO.File_Type;
      Input_Stream : SIO.Stream_Access;
      B            : Byte;
   begin
      SIO.Open (Input_File, SIO.In_File, File_Name);

      Input_Stream := SIO.Stream (Input_File);
      while not SIO.End_Of_File (Input_File) loop
         Byte'Read (Input_Stream, B);
         Buffer.Append (B);
      end loop;

      SIO.Close (Input_File);

      return Buffer;
   end Read_All_Bytes;

   -- Converts a Byte value into its hexadecimal two-digit string representation.
   -- Influenced by: https://ada.tips/using-adasequential_io-to-create-simple-hexdump-utility.html
   function Hex (B : Byte) return String is
      Hex_Digits  : constant array (Byte range 0 .. 15) of Character :=
        ('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D',
         'E', 'F');
      Low_Nybble  : constant Byte := B mod 16;
      High_Nybble : constant Byte                                    := B / 16;
   begin
      return Hex_Digits (High_Nybble) & Hex_Digits (Low_Nybble);
   end Hex;

   -- Generates a hex dump of the byte vector as a string.
   function Hex_Dump (Data : Byte_Vector) return String is
      Result : Unbounded_String := Null_Unbounded_String;
   begin
      for B of Data loop
         Result := Result & Hex (B) & " ";
      end loop;
      return To_String (Result);
   end Hex_Dump;

   type Note_Index is range 0 .. 11;

   Note_Names : constant array (Note_Index) of Note_Name :=
     ("C ", "C#", "D ", "D#", "E ", "F ", "F#", "G ", "G#", "A ", "A#", "B ");

   type Octave_Type is range -2 .. 8;

   function Name (Note_Number : in MIDI_Note_Type) return Note_Name is
      Octave_Offset : constant Integer     := -2;
      Octave        : constant Octave_Type :=
        Octave_Type ((Integer (Note_Number) / 12) + Octave_Offset);
   begin
      return Note_Names (Note_Index (Note_Number mod 12)) & Octave'Image;
   end Name;

end Sixten;
