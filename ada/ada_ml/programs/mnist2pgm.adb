pragma Ada_2022;

with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Integer_Text_IO;   use Ada.Integer_Text_IO;
with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
with Ada.Command_Line;      use Ada.Command_Line;
with MNIST_Reader;          use MNIST_Reader;

procedure MNIST2PGM is

   pragma Style_Checks (Off);
   
   Header : MNIST_Dataset_Header_Type;
   File   : Ada.Streams.Stream_IO.File_Type;
   
begin
   for I in 1 .. Argument_Count loop
      Open (File, In_File, Argument (I));      
      
      Get_Header (File, Header);
      
      declare
         Dimensions : MNIST_Dataset_Dimension_Array (1 .. Integer (Header.NDim));
         Image_Size : Positive := 1;
      begin
         Get_Dimensions (File, Dimensions);
         
         for I in 2 .. Header.NDim loop
            Image_Size := Image_Size * Positive (Dimensions (Integer (I)));
         end loop;
      
         declare
            type MNIST_Record_Raster is array (Positive range <>) of Byte;

            MNIST_Record : MNIST_Record_Raster (1 .. Image_Size);
            
            procedure Put_Record_As_PNG
              (
               R : MNIST_Record_Raster
              ) is
               K : Natural := R'First;
               D1 : Integer := 1;
               D2 : Integer := 1;
            begin
               if Dimensions'Length > 1 then
                  D1 := Integer (Dimensions (2));
               end if;
               
               if Dimensions'Length > 2 then
                  for DI in 3 .. Dimensions'Last loop
                     D2 := D2 * Integer (Dimensions (DI));
                  end loop;
               end if;
               
               Put_Line ("P2");
               Put_Line (D2'Image & " " & D1'Image);
               Put_Line ("255");
               for I in 1 .. D1 loop
                  for I in 1 .. D2 loop
                     Put (Integer (R (K)), 4);
                     Put (" ");
                     K := K + 1;
                  end loop;
                  New_Line;
               end loop;
            end;
            
            Input  : Stream_Access := Stream (File);

         begin
            MNIST_Record_Raster'Read (Input, MNIST_Record);
            Put_Record_As_PNG (MNIST_Record);
         end;
      end;
      
      Close (File);
   end loop;
end;
