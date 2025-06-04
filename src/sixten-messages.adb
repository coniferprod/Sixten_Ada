with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Sixten.Manufacturers; use Sixten.Manufacturers;

package body Sixten.Messages is
   procedure Put_Labeled_Number (Message : in String; Value: in Integer) is
   begin
      Ada.Text_IO.Put (Message);
      Ada.Integer_Text_IO.Put (Value, Width => 1);
      Ada.Text_IO.New_Line;
   end Put_Labeled_Number;

   Real_Time_Identifier     : constant Byte := 16#7F#;
   Non_Real_Time_Identifier : constant Byte := 16#7E#;

   procedure Parse (Data : Byte_Array; Message : out Message_Type) is
      -- The most common index for payload start (manufacturer-specific, standard)
      Payload_Start_Index : Natural := 2;

      -- The last byte before the System Exclusive terminator
      Payload_End_Index   : constant Natural := Data'Length - 1;

      --  Initialize offset to where we start looking for manufacturer ID
      Offset : Natural := Payload_Start_Index;

   begin
      if Data (1) /= Initiator then
         raise Message_Error
           with "System Exclusive Message must start with initiator "
               & Hex (Initiator) & " (hex)";
      end if;

      if Data (Data'Length) /= Terminator then
         raise Message_Error
           with "System Exclusive Message must end with terminator "
               & Hex (Terminator) & " (hex)";
      end if;

      case Data (Offset) is
         when Real_Time_Identifier | Non_Real_Time_Identifier =>
            Payload_Start_Index := Offset + 4;
            declare
               Payload_Size : Natural := Payload_End_Index - Offset;
               Payload : Byte_Array (1 .. Payload_Size);
            begin
               Payload := Data (Payload_Start_Index .. Payload_End_Index);
               Put_Labeled_Number ("Payload_Start_Index = ", Payload_Start_Index);
               Put_Labeled_Number ("Payload_End_Index = ", Payload_End_Index);
               Put_Labeled_Number ("Payload_Size = " , Payload_Size);
               Message :=
                  (Kind      => Universal, Payload_Size => Payload'Length,
                   Real_Time => (Data (Offset) = Real_Time_Identifier),
                   Target    => Data (Offset + 1), Sub_Status_1 => Data (Offset + 2),
                   Sub_Status_2      => Data (Offset + 3), Payload => Payload);
            end;
         when others =>
            declare
               Payload_Size : Natural := Payload_End_Index - Offset;
               Manufacturer_Data : Byte_Array (1 .. 3);
               Payload : Byte_Array (1 .. Payload_Size);
               Manufacturer : Manufacturer_Type;
            begin
               Manufacturer_Data := Data (Offset .. Offset + 2);
               Parse (Manufacturer_Data, Manufacturer);

               Payload_Start_Index := (case Kind (Manufacturer) is
                  when Normal => Offset + 1,
                  when Extended => Offset + 3);

               Payload := Data (Payload_Start_Index .. Payload_End_Index);
               Put_Labeled_Number ("Payload_Start_Index = ", Payload_Start_Index);
               Put_Labeled_Number ("Payload_End_Index = ", Payload_End_Index);
               Put_Labeled_Number ("Payload_Size = " , Payload_Size);

               Message := (Kind => Manufacturer_Specific, Payload_Size => Payload'Length,
                  Payload => Payload, Manufacturer => Manufacturer);
            end;
      end case;
   end Parse;

   function Payload_Length (Message : Message_Type) return Natural is
   begin
      return Natural (Message.Payload'Length);
   end Payload_Length;

   procedure Emit (Message : Message_Type; Result : out Byte_Vector) is
   begin
      Result.Append (Initiator);
      case Message.Kind is
         when Universal =>
            if Message.Real_Time then
               Result.Append (Real_Time_Identifier);
            else
               Result.Append (Non_Real_Time_Identifier);
            end if;
            Result.Append (Message.Target);
            Result.Append (Message.Sub_Status_1);
            Result.Append (Message.Sub_Status_2);
         when Manufacturer_Specific =>
            declare
               M_Bytes : constant Byte_Array := Sixten.Manufacturers.To_Bytes (Message.Manufacturer);
               M_BV : constant Byte_Vector := Sixten.To_Byte_Vector (M_Bytes);
            begin
               Result.Append_Vector (M_BV);
            end;
      end case;
      Result.Append_Vector (To_Byte_Vector (Message.Payload));
      Result.Append (Terminator);
   end Emit;

end Sixten.Messages;
