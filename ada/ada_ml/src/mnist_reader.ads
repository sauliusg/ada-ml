with System; use System;

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
   
   -- type MNIST_Dataset_Value_Type is array (1 .. 4) of Byte;
   type MNIST_Dataset_Value_Type is new DWord;

   type MNIST_Dataset_Record_Type is record
      Value : MNIST_Dataset_Value_Type;
   end record;
   
   for MNIST_Dataset_Record_Type'Bit_Order use High_Order_First;
   for MNIST_Dataset_Record_Type'Scalar_Storage_Order use High_Order_First;
   
   function N_Dimensions (Header : MNIST_Dataset_Header_Type) return Natural;
   
end;
