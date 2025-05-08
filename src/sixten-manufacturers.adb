with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;

package body Sixten.Manufacturers is

   procedure Parse (Data : in Byte_Array; Result : out Manufacturer_Type) is
   begin
      --Ada.Text_IO.Put ("Parse ==> Manufacturer_Type: ");
      --Ada.Text_IO.Put ("Data.First_Element = ");
      --Ada.Text_IO.Put (Hex (Data.First_Element));

      --Ada.Text_IO.Put (" Data.Last_Element = ");
      --Ada.Text_IO.Put (Hex (Data.Last_Element));
      --Ada.Text_IO.New_Line;

      if Data (0) = 0 then
         Result := (Extended, Identifier_1 => Data (1), Identifier_2 => Data (2));
      else
         Result := (Normal, Identifier => Data (0));
      end if;
   end Parse;

   -- Converts the manufacturer ID to a byte array for inclusion in a
   -- manufacturer-specific MIDI System Exclusive message.
   function To_Bytes (Manufacturer : Manufacturer_Type) return Byte_Array is
   begin
      case Manufacturer.Kind is
         when Normal => return (0 => Manufacturer.Identifier);
         when Extended => return (0, Manufacturer.Identifier_1, Manufacturer.Identifier_2);
      end case;
   end To_Bytes;

   function Manufacturer (B : Byte) return Manufacturer_Type is
   begin
      --  If there is only one byte, it must not be zero.
      if B = 0 then
         raise Manufacturer_Error;
      end if;

      return (Normal, Identifier => B);
   end Manufacturer;

   function Manufacturer
     (B1 : Byte; B2 : Byte; B3 : Byte) return Manufacturer_Type
   is
   begin
      if B1 /= 0 then
         raise Manufacturer_Error;
      end if;

      return (Extended, Identifier_1 => B2, Identifier_2 => B3);
   end Manufacturer;

   --  Makes a key to the hash map of manufacturers from the bytes.
   function Make_Key (Manufacturer : Manufacturer_Type) return String is
   begin
      case Manufacturer.Kind is
         when Normal => return Hex (Manufacturer.Identifier);
         when Extended =>
            return Hex (0) & Hex (Manufacturer.Identifier_1) & Hex (Manufacturer.Identifier_2);
      end case;
   end Make_Key;

   function Name (Manufacturer : Manufacturer_Type) return String
   is
      package Manufacturer_Maps is new Ada.Containers.Indefinite_Hashed_Maps
        (Key_Type => String, Element_Type => String, Hash => Ada.Strings.Hash,
         Equivalent_Keys => "=");
      M   : Manufacturer_Maps.Map;
      Key : constant String := Make_Key (Manufacturer);
   begin
      M.Include ("18", "E-mu");

      M.Include ("00206B", "Arturia");
      M.Include ("002109", "Native Instruments");

      M.Include ("40", "Kawai Musical Instruments MFG. CO. Ltd");
      M.Include ("41", "Roland Corporation");
      M.Include ("42", "Korg Inc.");
      M.Include ("43", "Yamaha");
      M.Include ("44", "Casio Computer Co. Ltd");
      -- 0x45 is not assigned
      M.Include ("46", "Kamiya Studio Co. Ltd");
      M.Include ("47", "Akai Electric Co. Ltd.");

      M.Include ("7D", "Development / Non-commercial");

      if M.Contains (Key) then
         return M (Key);
      else
         return "Unknown";
      end if;
   end Name;

   function Kind (Manufacturer : Manufacturer_Type) return Manufacturer_Kind is
   begin
      case Manufacturer.Kind is
         when Normal => return Normal;
         when Extended => return Extended;
      end case;
   end Kind;

end Sixten.Manufacturers;
