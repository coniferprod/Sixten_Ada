pragma Assertion_Policy (Check);

package Sixten.Manufacturers is

   -- Would have used "Standard" instead of normal, but it
   -- causes trouble because it is the name of an Ada package.
   -- Using "Normal" instead.
   type Manufacturer_Kind is (Normal, Extended);

   type Manufacturer_Type is private;

   -- Parses a byte array into a manufacturer ID. The data must always have
   -- at least three bytes, though all may not be used.
   procedure Parse (Data : in Byte_Array; Result : out Manufacturer_Type) with
     Pre => Data'Length >= 3;

   -- Converts the manufacturer ID to a byte array for inclusion in a
   -- manufacturer-specific MIDI System Exclusive message.
   function To_Bytes (Manufacturer : Manufacturer_Type) return Byte_Array;

   --  Makes a normal manufacturer from one byte.
   function Manufacturer (B : Byte) return Manufacturer_Type;

   --  Makes an extended manufacturer from three bytes.
   function Manufacturer
     (B1 : Byte; B2 : Byte; B3 : Byte) return Manufacturer_Type; -- extended

   --  Gets the name of the manufacturer.
   function Name (Manufacturer : Manufacturer_Type) return String;

   --  Gets the kind of the manufacturer.
   function Kind (Manufacturer : Manufacturer_Type) return Manufacturer_Kind;

   --  Development identifier
   Development        : constant Manufacturer_Type;

   --  Some ready-made manufacturers for convenience.
   Kawai              : constant Manufacturer_Type;
   Native_Instruments : constant Manufacturer_Type;
   Roland             : constant Manufacturer_Type;
   Yamaha : constant Manufacturer_Type;

   Manufacturer_Error : exception;

private
   -- Use a variant record to describe the manufacturer
   -- in a MIDI System Exclusive Message.
   type Manufacturer_Type (Kind : Manufacturer_Kind := Normal) is
   record
      case Kind is
         when Normal =>
            Identifier : Byte;
         when Extended =>
            Identifier_1 : Byte;
            Identifier_2 : Byte;
      end case;
   end record;

   Development        : constant Manufacturer_Type := (Normal, 16#7D#);

   Kawai              : constant Manufacturer_Type := (Normal, 16#40#);
   Native_Instruments : constant Manufacturer_Type := (Extended, 16#21#, 16#09#);
   Roland             : constant Manufacturer_Type := (Normal, 16#41#);
   Yamaha             : constant Manufacturer_Type := (Normal, 16#43#);

end Sixten.Manufacturers;
