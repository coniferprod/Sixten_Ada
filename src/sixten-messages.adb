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
      --  Offset where we start looking for manufacturer ID
      Offset : Natural;

      --  Shortest possible message is "F0 40 xx F7" (standard manufacturer,
      --  one byte of payload)

      Payload_Start_Index : Natural;
      Payload_End_Index : constant Natural := Data'Last - 1;

      Payload_Size : Natural;
   begin
      declare
         First_Byte : constant Byte := Data (Data'First);
         Last_Byte  : constant Byte := Data (Data'Last);
      begin
         if First_Byte /= Initiator then
            raise Message_Error
            with "System Exclusive Message must start with initiator "
                  & Hex (Initiator) & " (hex)";
         end if;

         if Last_Byte /= Terminator then
            raise Message_Error
            with "System Exclusive Message must end with terminator "
                  & Hex (Terminator) & " (hex)";
         end if;
      end;

      Offset := 1;
      case Data (Offset) is
         when Real_Time_Identifier | Non_Real_Time_Identifier =>
            Payload_Start_Index := 5;
            if Payload_Start_Index >= Payload_End_Index then --  no payload
               Payload_Size := 0;
            else
               Payload_Size := Payload_End_Index - Payload_Start_Index;
            end if;
            if Debugging then
               Put_Labeled_Number ("Payload_Start_Index = ", Payload_Start_Index);
               Put_Labeled_Number ("Payload_End_Index = ", Payload_End_Index);
               Put_Labeled_Number ("Payload_Size = ", Payload_Size);
            end if;
            declare
               Payload : Byte_Array (1 .. Payload_Size);  --  could be a null range
               Payload_Offset : Natural;  -- payload starts here in the message data
            begin
               Payload_Offset := Payload_Start_Index;
               for I in Payload'Range loop   --  does nothing if null range
                  Payload (I) := Data (I + Payload_Offset);
               end loop;

               Message :=
                  (Kind      => Universal, 
                   Payload_Size => Payload'Length,
                   Real_Time => (Data (1) = Real_Time_Identifier),
                   Target    => Data (2), 
                   Sub_Status_1 => Data (3),
                   Sub_Status_2 => Data (4), 
                   Payload => Payload);
            end;
         when others =>
            Payload_Start_Index := 2;
            Payload_Size := Payload_End_Index - Payload_Start_Index;
            if Debugging then
               Put_Labeled_Number ("Payload_Start_Index = ", Payload_Start_Index);
               Put_Labeled_Number ("Payload_End_Index = ", Payload_End_Index);
               Put_Labeled_Number ("Payload_Size = ", Payload_Size);
            end if;
            declare
               --  The message is at least four bytes, so there will be 
               --  three bytes of tentative manufacturer data.
               Manufacturer_Data : constant Byte_Array (1 .. 3) := Data (1 .. 3);

               Payload_Offset : Natural;  -- payload starts here in the message data

               Payload : Byte_Array (1 .. Payload_Size);
               Manufacturer : Manufacturer_Type;
            begin
               Parse (Manufacturer_Data, Manufacturer);

               --  Standard manufacturer: F0 40 xx F7
               --  Extended manufacturer: F0 00 20 6B xx F7
               Payload_Offset := (case Kind (Manufacturer) is
                  when Normal => 2,
                  when Extended => 4);

               for I in Payload'Range loop
                  Payload (I) := Data (I + Payload_Offset);
               end loop;

               Message := (Kind => Manufacturer_Specific, 
                           Payload_Size => Payload'Length,
                           Payload => Payload, 
                           Manufacturer => Manufacturer);
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
               M_Bytes : constant Byte_Array := To_Bytes (Message.Manufacturer);
               M_BV : constant Byte_Vector := Sixten.To_Byte_Vector (M_Bytes);
            begin
               Result.Append_Vector (M_BV);
            end;
      end case;
      Result.Append_Vector (To_Byte_Vector (Message.Payload));
      Result.Append (Terminator);
   end Emit;

end Sixten.Messages;
