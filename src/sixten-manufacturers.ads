pragma Assertion_Policy (Check);

package Sixten.Manufacturers is

   -- Would have used "Standard" instead of normal, but it
   -- causes trouble because it is the name of an Ada package.
   -- Using "Normal" instead.
   type Manufacturer_Kind is (Normal, Extended);

   type Manufacturer_Type is private;

   Development_Identifier : constant Byte := 16#7D#;

   -- Parses a byte array into a manufacturer ID. The data must always have
   -- at least three bytes.
   procedure Parse (Data : in Byte_Array; Result : out Manufacturer_Type) with
     Pre => Data'Length >= 3;

   function To_Bytes (Manufacturer : Manufacturer_Type) return Byte_Array;

   function Manufacturer (B : Byte) return Manufacturer_Type;  -- normal
   function Manufacturer
     (B1 : Byte; B2 : Byte; B3 : Byte) return Manufacturer_Type; -- extended

   function Name (Manufacturer : Manufacturer_Type) return String;
   function Kind (Manufacturer : Manufacturer_Type) return Manufacturer_Kind;

   Yamaha : constant Manufacturer_Type;
   Kawai              : constant Manufacturer_Type;
   Native_Instruments : constant Manufacturer_Type;
   Development        : constant Manufacturer_Type;

   Manufacturer_Error : exception;

   function Make_Key (Manufacturer : Manufacturer_Type) return String;

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

   Development        : constant Manufacturer_Type := (Normal, Development_Identifier);

   Kawai              : constant Manufacturer_Type := (Normal, 16#40#);
   Native_Instruments : constant Manufacturer_Type := (Extended, 16#21#, 16#09#);
   Yamaha             : constant Manufacturer_Type := (Normal, 16#43#);

end Sixten.Manufacturers;
