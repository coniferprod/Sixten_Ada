with Ada.Text_IO;
with Sixten.Manufacturers; use Sixten.Manufacturers;

package body Sixten.Messages is

   Real_Time_Identifier     : constant Byte := 16#7F#;
   Non_Real_Time_Identifier : constant Byte := 16#7E#;

   procedure Parse (Data : Byte_Vector; Message : out Message_Type) is
      Manufacturer        : Manufacturer_Type;
      Payload_Start_Index : Natural          :=
        1;  -- the most common index for payload start (manufacturer-specific, standard)
      Payload_End_Index   : constant Natural :=
        Data.Last_Index - 1;  -- last byte before SysEx terminator
      Payload             : Byte_Vector;
      Offset              : constant Natural :=
        1; -- where we start looking for manufacturer ID
   begin
      --Ada.Text_IO.Put_Line ("Parse ==> MessageType: ");
      --Ada.Text_IO.Put ("Data.First_Element = ");
      --Ada.Text_IO.Put (Hex (Data.First_Element));
      --Ada.Text_IO.New_Line;

      --Ada.Text_IO.Put ("Data.Last_Element = ");
      --Ada.Text_IO.Put (Hex (Data.Last_Element));
      --Ada.Text_IO.New_Line;

      if Data.First_Element /= Initiator then
         raise Message_Error
           with "System Exclusive Message must start with initiator "
               & Hex (Initiator) & " (hex)";
      end if;

      if Data.Last_Element /= Terminator then
         raise Message_Error
           with "System Exclusive Message must end with terminator "
               & Hex (Terminator) & " (hex)";
      end if;

      case Data (Offset) is
         when Real_Time_Identifier | Non_Real_Time_Identifier =>
            Payload_Start_Index := Offset + 4;
            for I in Payload_Start_Index .. Payload_End_Index loop
               Payload.Append (Data (I));
            end loop;

            Message :=
              (Kind      => Universal,
               Real_Time => (Data (Offset) = Real_Time_Identifier),
               Target    => Data (Offset + 1), Sub_Status_1 => Data (Offset + 2),
               Sub_Status_2      => Data (Offset + 3), Payload => Payload);
         when others =>
            declare
               Manufacturer_Data : Byte_Array (0 .. 2);
            begin
               Manufacturer_Data := (Data (Offset), Data (Offset + 1), Data (Offset + 2));
               Parse (Manufacturer_Data, Manufacturer);

               Payload_Start_Index := (case Kind (Manufacturer) is
                  when Normal => Offset + 1,
                  when Extended => Offset + 3);
               --Ada.Text_IO.Put_Line ("Payload_Start_Index = " & Natural'Image (Payload_Start_Index));
               --Ada.Text_IO.Put_Line ("Payload_End_Index = " & Natural'Image (Payload_End_Index));

               for I in Payload_Start_Index .. Payload_End_Index loop
                  Payload.Append (Data (I));
                  --Ada.Text_IO.Put_Line ("I = " & Integer'Image (I));
               end loop;
            end;

            Message := (Manufacturer_Specific, Payload, Manufacturer);
      end case;
      --Ada.Text_IO.Put_Line ("exiting from Parse");
   end Parse;

   function Payload_Length (Message : Message_Type) return Natural is
   begin
      return Natural (Message.Payload.Length);
   end Payload_Length;

   procedure Emit (Message : Message_Type; Result : out Byte_Vector) is
   begin
      Result.Append (Initiator);
      case Message.Kind is
         when Universal =>
            if Message.Real_Time then
               Result.Append (16#7E#);
            else
               Result.Append (16#7F#);
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
      Result.Append_Vector (Message.Payload);
      Result.Append (Terminator);
   end Emit;

end Sixten.Messages;
