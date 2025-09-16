with Interfaces;
with Ada.Containers;
with Ada.Containers.Vectors;
use type Ada.Containers.Count_Type;
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

   function To_Byte_Array (V : Byte_Vector) return Byte_Array;
   function To_Byte_Vector (Data : Byte_Array) return Byte_Vector;

   procedure Delete is new Ada.Unchecked_Deallocation
     (Byte_Array, Byte_Array_Access);

   function Read_All_Bytes (File_Name : String) return Byte_Vector;
   function Hex (B : Byte) return String;
   function Hex_Dump (Data : Byte_Vector) return String;

   type Nybble_Order_Type is (High_First, Low_First);
   function Denybblify (Buffer : in Byte_Vector; 
                        Order : in Nybble_Order_Type := High_First) return Byte_Vector
      with Pre => Buffer.Length mod 2 = 0;
   function Nybblify (Buffer : in Byte_Vector; Order : in Nybble_Order_Type := High_First) return Byte_Vector;
   
   procedure Write_File (Name : String; Contents : Byte_Vector);
   procedure Read_File (Name : String; Contents : out Byte_Array);

   -- MIDI note number (7 bits)
   subtype MIDI_Note_Type is Integer range 0 .. 127;

   -- MIDI channel number
   type MIDI_Channel_Type is range 1 .. 16;

   -- Name of MIDI note.
   subtype Note_Name is String (1 .. 2);

   function Name (Note_Number : in MIDI_Note_Type) return Note_Name;

   Debugging : Boolean := False;

end Sixten;
