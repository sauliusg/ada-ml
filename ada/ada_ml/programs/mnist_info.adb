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
      
      declare
         type MNIST_Dataset_Dimension_Array is array (1 .. Header.NDim) of
           MNIST_Dataset_Record_Type;
           
         Dimensions : MNIST_Dataset_Dimension_Array;
         
         function To_DWord (W : MNIST_Dataset_Record_Type) return DWord is
            R : DWord := 0;
         begin
            for I in 1 .. 4 loop
               R := R * 256 + DWord ((W / (256 ** (I - 1))) and 255);
            end loop;
            return R;
         end;
 
      begin
         MNIST_Dataset_Dimension_Array'Read (Input, Dimensions);
         for I in Dimensions'Range loop
            Put_Line ("Dim " & I'Image & " : " & 
                        To_DWord (Dimensions (I))'Image);
         end loop;
      end;
      
      Close (File);
   end loop;
end;
