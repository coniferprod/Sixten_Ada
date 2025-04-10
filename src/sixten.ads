with Interfaces;
with Ada.Containers;
with Ada.Containers.Vectors;

package Sixten is

   type Byte is new Interfaces.Unsigned_8;

   type Byte_Array is array (Natural range <>) of Byte;

   package Byte_Vectors is new Ada.Containers.Vectors
     (Index_Type => Natural, Element_Type => Byte);

   subtype Byte_Vector is Sixten.Byte_Vectors.Vector;

   function Read_All_Bytes (File_Name : String) return Byte_Vector;
   function Hex (B : Byte) return String;
end Sixten;
