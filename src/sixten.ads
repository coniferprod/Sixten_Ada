with Interfaces;
with Ada.Containers;
with Ada.Containers.Vectors;
with Ada.Sequential_IO;
with Ada.Unchecked_Deallocation;

package Sixten is

   -- Define a type for an 8-bit byte compatible with the outside world.
   type Byte is new Interfaces.Unsigned_8;

   -- An array of 8-bit bytes.
   type Byte_Array is array (Natural range <>) of Byte;
   type Byte_Array_Access is access Byte_Array;

   package Byte_Vectors is new Ada.Containers.Vectors
     (Index_Type => Natural, Element_Type => Byte);
   subtype Byte_Vector is Sixten.Byte_Vectors.Vector;
   package Byte_IO is new Ada.Sequential_IO (Byte);

   function To_Byte_Array (V : Byte_Vector) return Byte_Array;
   function To_Byte_Vector (Data : Byte_Array) return Byte_Vector;

   procedure Delete is new Ada.Unchecked_Deallocation
     (Byte_Array, Byte_Array_Access);

   function Read_All_Bytes (File_Name : String) return Byte_Vector;
   function Hex (B : Byte) return String;
   function Hex_Dump (Data : Byte_Vector) return String;

   procedure Write_File (Name : String; Contents : Byte_Vector);
   procedure Read_File (Name : String; Contents : out Byte_Array);

   -- MIDI note number (7 bits)
   type MIDI_Note_Type is range 0 .. 127;

   -- MIDI channel number
   type MIDI_Channel_Type is range 1 .. 16;

   -- Name of MIDI note.
   subtype Note_Name is String (1 .. 2);

   function Name (Note_Number : in MIDI_Note_Type) return Note_Name;

end Sixten;
