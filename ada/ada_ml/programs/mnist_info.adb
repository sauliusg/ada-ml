pragma Ada_2022;

with Ada.Text_IO;      use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with MNIST_Reader;     use MNIST_Reader;
with Ada.Streams;      use Ada.Streams;
with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;

procedure MNIST_Info is

   pragma Style_Checks (Off);
   
   Header : MNIST_Dataset_Header_Type;
   File   : Ada.Streams.Stream_IO.File_Type;
   Input  : Stream_Access;
   
begin
   for I in 1 .. Argument_Count loop
      Open (File, In_File, Argument (I));      
      Input := Stream (File);
      
      MNIST_Dataset_Header_Type'Read (Input, Header);
      
      Put_Line ("Empty : " & Header.Empty'Image);
      Put_Line ("DType : " & Header.Data_Type'Image);
      Put_Line ("NDim  : " & Header.NDim'Image);
      
      if False then
         declare
            type MNIS_Dimension_Type is
              array (1 .. Header.NDim) of MNIST_Dataset_Record_Type;
         
            Dimensions : MNIS_Dimension_Type;
         begin
            MNIS_Dimension_Type'Read (Input, Dimensions);
         
            for I in Dimensions'Range loop
               Put_Line ("Dim " & I'Image & " : " & Dimensions (I).Value'Image);
            end loop;
         end;
      else
         declare
            Dimension : MNIST_Dataset_Record_Type;
            function To_DWord (W : MNIST_Dataset_Record_Type) return DWord is
               R : DWord := 0;
            begin
               for I in 1 .. 4 loop
                  R := R * 256 + DWord ((W.Value / (256 ** (I - 1))) and 255);
               end loop;
               return R;
            end;
         begin
            for I in 1 .. Header.NDim loop
               MNIST_Dataset_Record_Type'Read (Input, Dimension);
               Put_Line ("Dim " & I'Image & " : " & To_DWord (Dimension)'Image);
            end loop;
         end;
      end if;
      
      Close (File);
   end loop;
end;
