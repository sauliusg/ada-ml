with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;

package MNIST_Reader is

   pragma Style_Checks (Off);
   
   type Byte is mod 2**8;
   type Word is mod 2**16;
   type DWord is mod 2**32;
   
   type MNIST_Dataset_Header_Type is record
      Empty : Word;     -- should be filled with zeroes;
      Data_Type : Byte; -- 0x08: unsigned byte, ... (https://github.com/cvdfoundation/mnist)
      NDim : Byte;
   end record;
   
   type MNIST_Dataset_Dimension_Array is array (Positive range <>) of DWord;
   
   function To_DWord (W : DWord) return DWord;

   function N_Dimensions (Header : MNIST_Dataset_Header_Type) return Natural;
   
   procedure Get_Header
     (
      File : File_Type;
      Header : out MNIST_Dataset_Header_Type
     );
   
   procedure Get_Dimensions
     (
      File : File_Type;
      Dimensions : out MNIST_Dataset_Dimension_Array
     );
   
end;
