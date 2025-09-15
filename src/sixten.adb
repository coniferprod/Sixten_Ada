with Ada.Streams.Stream_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body Sixten is

   package Byte_IO is new Ada.Sequential_IO (Byte);

   -- Helper function to increment an integer value.
   function Inc (I : in Integer; Amount : in Integer := 1) return Integer is (I + Amount)
      with Inline;

   -- Helper function to decrement an integer value.
   function Dec (I : in Integer; Amount : in Integer := 1) return Integer is (I - Amount)
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
         BV.Append (Data (I));
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
      Index        : Natural := Contents'First;
      B            : Byte;
   begin
      SIO.Open (Input_File, SIO.In_File, Name);

      Input_Stream := SIO.Stream (Input_File);
      while not SIO.End_Of_File (Input_File) loop
         Byte'Read (Input_Stream, B);
         Contents (Index) := B;
         Index            := Index + 1;
      end loop;

      SIO.Close (Input_File);
   end Read_File;

   function Read_All_Bytes (File_Name : String) return Byte_Vector is
      Buffer       : Byte_Vector;
      Input_File   : Byte_IO.File_Type;
      Value        : Byte;
   begin
      Byte_IO.Open (Input_File, Byte_IO.In_File, File_Name);

      while not Byte_IO.End_Of_File (Input_File) loop
         Byte_IO.Read (Input_File, Value);
         Buffer.Append (Value);
      end loop;

      Byte_IO.Close (Input_File);

      return Buffer;
   end Read_All_Bytes;

   --  Gets the high nybble from a byte.
   function High_Nybble (Value : in Byte) return Byte is
      Temp : Byte;
   begin
      Temp := Value and 16#F0#;
      Temp := Shift_Right (Temp, 4);
      return Temp;
   end High_Nybble;

   --  Gets the low nybble from a byte.
   function Low_Nybble (Value : in Byte) return Byte is
   begin
      return Value and 16#0F#;
   end Low_Nybble;

   function Denybblify (Buffer : in Byte_Vector; Order : in Nybble_Order_Type := High_First) return Byte_Vector 
      is

      function Byte_From_Nybbles (High : Byte; Low : Byte) return Byte is
         Temp : Byte;
      begin
         Temp := Shift_Left (High, 4);
         return Temp or Low;
      end Byte_From_Nybbles;

      Result : Byte_Vector;
      Value, High, Low : Byte;
      Index : Integer := 0;
      Offset : Integer := 0;
      Count : constant Ada.Containers.Count_Type := Buffer.Length / 2;
   begin
      while Index < Integer (Count) loop
         High := Buffer.Element (Offset);
         Low := Buffer.Element (Offset + 1);

         if Order = High_First then
            Value := Byte_From_Nybbles (High, Low);
         else
            Value := Byte_From_Nybbles (Low, High);
         end if;

         Result.Append (Value);
         Index := Inc (Index);
         Offset := Inc (Offset, 2);
      end loop;

      return Result;
   end Denybblify;

   function Nybblify (Buffer : in Byte_Vector; Order : in Nybble_Order_Type := High_First) return Byte_Vector is
      Result : Byte_Vector;
      H, L : Byte;
   begin
      for Value of Buffer loop
         H := High_Nybble (Value);
         L := Low_Nybble (Value);
         if Order = High_First then
            Result.Append (H);
            Result.Append (L);
         else
            Result.Append (L);
            Result.Append (H);
         end if;
      end loop;
      return Result;
   end Nybblify;

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
      return To_String (Trim (Source => Result, Side => Ada.Strings.Both));
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
