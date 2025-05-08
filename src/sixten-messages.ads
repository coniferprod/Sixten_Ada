with Sixten.Manufacturers;

package Sixten.Messages is
   subtype Payload_Size_Type is Natural range 0 .. 2**19;

   type Message_Kind is (Universal, Manufacturer_Specific);

   type Message_Type (Kind : Message_Kind := Manufacturer_Specific; Payload_Size : Payload_Size_Type := 0) is record
      Payload : Byte_Array (1 .. Payload_Size);
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

   --procedure Parse (Data : Byte_Vector; Message : out Message_Type);
   procedure Parse (Data : Byte_Array; Message : out Message_Type);

   --  Gets the length of the message payload. It is expected that there
   --  is always some payload, but the return type is Natural because the
   --  Count_Type defined in Ada.Containers starts from zero (otherwise
   --  this function could return Positive).
   function Payload_Length (Message : Message_Type) return Natural;

   procedure Emit (Message : Message_Type; Result : out Byte_Vector);

end Sixten.Messages;
