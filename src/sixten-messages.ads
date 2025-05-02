with Sixten.Manufacturers;

package Sixten.Messages is

   type Message_Kind is (Universal, Manufacturer_Specific);

   type Message_Type (Kind : Message_Kind := Manufacturer_Specific) is record
      Payload : Byte_Vector;
      case Kind is
         when Universal =>
            Real_Time : Boolean;
            Target    : Byte;
            Sub_Status_1      : Byte;
            Sub_Status_2      : Byte;
         when Manufacturer_Specific =>
            Manufacturer : Sixten.Manufacturers.Manufacturer_Type;
      end case;
   end record;

   Message_Error : exception;

   Initiator  : constant Byte := 16#F0#;
   Terminator : constant Byte := 16#F7#;

   procedure Parse (Data : Byte_Vector; Message : out Message_Type);
   function Payload_Length (Message : Message_Type) return Natural;
   procedure Emit (Message : Message_Type; Result : out Byte_Vector);

end Sixten.Messages;
