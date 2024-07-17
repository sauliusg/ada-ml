package body MNIST_Reader is

   pragma Style_Checks (Off);
   
   function N_Dimensions (Header : MNIST_Dataset_Header_Type) return Natural is
   begin
      return Natural (Header.NDim);
   end;
   
end;
