with Ada.Streams.Stream_IO;

package body Sixten is

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
   function Hex (B : Byte) return String is
      Hex_Digits  : constant array (Byte range 0 .. 15) of Character :=
        ('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D',
         'E', 'F');
      Low_Nybble  : constant Byte := B mod 16;
      High_Nybble : constant Byte                                    := B / 16;
   begin
      return Hex_Digits (High_Nybble) & Hex_Digits (Low_Nybble);
   end Hex;

end Sixten;
