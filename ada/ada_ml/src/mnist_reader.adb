with Ada.Text_IO.Text_Streams; use Ada.Text_IO.Text_Streams;

package body MNIST_Reader is

   pragma Style_Checks (Off);
   
   function To_DWord (W : DWord) return DWord is
      R : DWord := 0;
   begin
      for I in 1 .. 4 loop
         R := R * 256 + ((W / (256 ** (I - 1))) and 255);
      end loop;
      return R;
   end;

   function N_Dimensions (Header : MNIST_Dataset_Header_Type) return Natural is
   begin
      return Natural (Header.NDim);
   end;
   
   procedure Get_Header
     (
      File : File_Type;
      Header : out MNIST_Dataset_Header_Type
     ) is
      Input : constant Stream_Access := Stream (File);
   begin
      MNIST_Dataset_Header_Type'Read (Input, Header);
   end;
   
   procedure Get_Dimensions
     (
      File : File_Type;
      Dimensions : out MNIST_Dataset_Dimension_Array
     ) is
      Input : constant Stream_Access := Stream (File);      
   begin
      MNIST_Dataset_Dimension_Array'Read (Input, Dimensions);
      for I in Dimensions'Range loop
         Dimensions (I) := TO_DWord (Dimensions (I));
      end loop;
   end;
   
end;
