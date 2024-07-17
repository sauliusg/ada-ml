with Ada.Text_IO;      use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Exceptions;   use Ada.Exceptions;

with ONNX_Runtime.Environments;
with ONNX_Runtime.Sessions;
with ONNX_Runtime.Values;

with PNM_Reader;

procedure Ada_Ml is

   pragma Style_Checks (Off);
   
   type PNM_16bin_Pixel is mod 2**16;
   
   package PNM_16bit_Reader is new PNM_Reader (PNM_16bin_Pixel);
   
   use PNM_16bit_Reader;
   
   -- -------------------------------------------------------------------------
   
   o : constant Float := 0.0;
   X : constant Float := 1.0;

   s28x28 : constant := 28 * 28;

   pragma Style_Checks (Off);
   Image : constant ONNX_Runtime.Values.Float_Array (1 .. s28x28) :=
     (o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,   --  1
      o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,   --  2
      o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,   --  3
      o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,   --  4
      o,o,o,o,o,o,X,X,X,X,X,X,X,X,X,X,o,o,o,o,o,o,o,o,o,o,o,o,   --  5
      o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,X,o,o,o,o,o,o,o,o,o,o,o,o,   --  6
      o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,X,o,o,o,o,o,o,o,o,o,o,o,o,   --  7
      o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,X,o,o,o,o,o,o,o,o,o,o,o,o,   --  8
      o,o,o,o,o,o,o,o,o,o,o,o,o,o,X,o,o,o,o,o,o,o,o,o,o,o,o,o,   --  9
      o,o,o,o,o,o,o,o,o,o,o,o,o,o,X,o,o,o,o,o,o,o,o,o,o,o,o,o,   --  10
      o,o,o,o,o,o,o,o,o,o,o,o,o,o,X,o,o,o,o,o,o,o,o,o,o,o,o,o,   --  11
      o,o,o,o,o,o,o,o,o,o,o,o,o,X,o,o,o,o,o,o,o,o,o,o,o,o,o,o,   --  12
      o,o,o,o,o,o,o,o,o,o,o,o,o,X,o,o,o,o,o,o,o,o,o,o,o,o,o,o,   --  13
      o,o,o,o,o,o,o,o,o,o,o,o,o,X,o,o,o,o,o,o,o,o,o,o,o,o,o,o,   --  14
      o,o,o,o,o,o,o,o,o,o,o,o,X,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,   --  15
      o,o,o,o,o,o,o,o,o,X,X,X,X,X,X,X,o,o,o,o,o,o,o,o,o,o,o,o,   --  16
      o,o,o,o,o,o,o,o,o,o,o,o,X,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,   --  17
      o,o,o,o,o,o,o,o,o,o,o,X,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,   --  18
      o,o,o,o,o,o,o,o,o,o,o,X,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,   --  19
      o,o,o,o,o,o,o,o,o,o,o,X,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,   --  20
      o,o,o,o,o,o,o,o,o,o,X,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,   --  21
      o,o,o,o,o,o,o,o,o,o,X,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,   --  22
      o,o,o,o,o,o,o,o,o,o,X,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,   --  23
      o,o,o,o,o,o,o,o,o,X,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,   --  24
      o,o,o,o,o,o,o,o,o,X,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,   --  25
      o,o,o,o,o,o,o,o,o,X,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,   --  26
      o,o,o,o,o,o,o,o,X,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,   --  27
      o,o,o,o,o,o,o,o,X,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o,o);  --  28
   
   -- pragma Style_Checks (On);
   
   Env : constant ONNX_Runtime.Environments.Environment :=
     ONNX_Runtime.Environments.Create_Environment;

   Output : ONNX_Runtime.Values.Value_Array (1 .. 1);

   Probability : ONNX_Runtime.Values.Float_Array (0 .. 9);
   Max         : ONNX_Runtime.Values.Element_Index := Probability'First;
   
   -- -------------------------------------------------------------------------

   function PNM_Raster_To_Array (Image : PNM_Image_Type)
                                return ONNX_Runtime.Values.Float_Array
   is
      use ONNX_Runtime.Values;
      
      Image_Size : constant Element_Index :=
        Element_Index(Image.Raster.M * Image.Raster.N);
      
      Retval : ONNX_Runtime.Values.Float_Array (1 .. Image_Size);
      
      K : Element_Index := 1;
   begin
      for I in Image.Raster.Pixels'Range (1) loop
         for J in Image.Raster.Pixels'Range (2) loop
            Retval (K) := Float (Image.Raster.Pixels (I, J));
            K := K + 1;
         end loop;
      end loop;
      return Retval;
   end;
   
   -- -------------------------------------------------------------------------
   
   NO_MODEL_PROVIDED : exception;
   
   No_Model_File_Status : constant Ada.Command_Line.Exit_Status := 1;
   
   Model_File_Name : constant String :=
     (if Argument_Count > 0 then Argument (1) else "");
   
begin
   
   if Argument_Count = 0 then
      raise NO_MODEL_PROVIDED with
        "a model file name must be provided " &
        "as the first command line argument";
   end if;
   
   declare
      Session : ONNX_Runtime.Sessions.Session :=
        Env.Create_Session (Model => Model_File_Name);
      --  https://github.com/microsoft/onnxruntime-inference-examples/
      --  raw/main/c_cxx/MNIST/mnist.onnx
      PNM_Image : PNM_Image_Type;
   begin

      for I in 2 .. Argument_Count loop
         
         Load_Raster (Argument (I), PNM_Image);
         
         declare
            Input : constant ONNX_Runtime.Values.Value_Array (1 .. 1) :=
              (1 => ONNX_Runtime.Values.Create_Tensor (Image, (1, 1, 28, 28)));
         begin
            Session.Run (Input, Output);
            Output (1).Get_Data (Probability);

            for J in Probability'Range loop
               if Probability (Max) < Probability (J) then
                  Max := J;
               end if;
            end loop;

            Ada.Text_IO.Put_Line ("Result:" & Max'Image);
         end;

      end loop;
      
   end;
   
exception
   when Exception_Occurence : NO_MODEL_PROVIDED =>
      Put_Line (Command_Name & ": " & Exception_Message(Exception_Occurence));
      Ada.Command_Line.Set_Exit_Status (No_Model_File_Status);
   
end Ada_Ml;
