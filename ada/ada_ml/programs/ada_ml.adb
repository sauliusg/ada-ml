pragma Ada_2022;

with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Float_Text_IO;   use Ada.Float_Text_IO;
with Ada.Command_Line;    use Ada.Command_Line;
with Ada.Exceptions;      use Ada.Exceptions;

with Ada.Numerics.Elementary_Functions;
use Ada.Numerics.Elementary_Functions;

with ONNX_Runtime.Environments;
with ONNX_Runtime.Sessions;
with ONNX_Runtime.Values; use ONNX_Runtime.Values;

with PNM_Reader;

with Ada.Environment_Variables;
with Ada.Characters.Handling;

procedure Ada_Ml is

   pragma Style_Checks (Off);
   
   type PNM_16bin_Pixel is mod 2**16;
   
   package PNM_16bit_Reader is new PNM_Reader (PNM_16bin_Pixel);
   
   use PNM_16bit_Reader;
   
   package OS_Environment renames Ada.Environment_Variables;
   
   -- -------------------------------------------------------------------------
   
   Env : constant ONNX_Runtime.Environments.Environment :=
     ONNX_Runtime.Environments.Create_Environment;

   Output : ONNX_Runtime.Values.Value_Array (1 .. 1);

   Probability : ONNX_Runtime.Values.Float_Array (0 .. 9);
   Max         : ONNX_Runtime.Values.Element_Index := Probability'First;
   
   -- -------------------------------------------------------------------------

   function PNM_Raster_To_Array (Image : PNM_Image_Type)
                                return ONNX_Runtime.Values.Float_Array
   is
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
     
   -- -------------------------------------------------------------------------
     
   procedure Put_PNM_Image (Image : PNM_Image_Type) is
      Log_Max : Float;
      N_Digits : Integer;
   begin
      Put_Line ("P2");
      Put_Line ("# " & Image.Format'Image (1 .. 2));
      Put (Image.Raster.M, 0);
      Put (" ");
      Put (Image.Raster.N, 0);
      New_Line;
      Put (Image.MaxVal, 0);
      New_Line;
      
      Log_Max := Log (Float (Image.MaxVal), Base => 10.0);
      N_Digits := Integer (Log_Max) + 1;
      -- Put_Line (">>> " & N_Digits'Image);

      for I in Image.Raster.Pixels'Range (1) loop
         for J in Image.Raster.Pixels'Range (2) loop
            Put (Integer (Image.Raster.Pixels (I, J)), N_Digits + 1);
         end loop;
         New_Line;
      end loop;
   end;
     
begin
   
   if Argument_Count = 0 then
      raise NO_MODEL_PROVIDED with
        "a model file name must be provided " &
        "as the first command line argument";
   end if;
   
   declare
      Session : ONNX_Runtime.Sessions.Session :=
        Env.Create_Session (Model => Model_File_Name);
      --  https://github.com/microsoft/onnxruntime-inference-examples/raw/main/c_cxx/MNIST/mnist.onnx
      PNM_Image : PNM_Image_Type;
      File : File_Type;
   begin

      for I in 2 .. Argument_Count loop
         
         Open (File, In_File, Argument (I));
         
         while not End_Of_File (File) loop
            Load_Raster (File, PNM_Image);
            if OS_Environment.Exists ("ADA_ML_DUMP_IMAGE") and then
              (
               OS_Environment.Value ("ADA_ML_DUMP_IMAGE") = "1" or else
                 Ada.Characters.Handling.To_Lower
                 (OS_Environment.Value ("ADA_ML_DUMP_IMAGE")) = "true"
              )
            then
               Put_PNM_Image (PNM_Image);
            end if;
         
            declare
               Input : constant ONNX_Runtime.Values.Value_Array (1 .. 1) :=
                 (1 => ONNX_Runtime.Values.Create_Tensor
                    (
                     PNM_Raster_To_Array (PNM_Image),
                     (1, 1,
                      Element_Index (PNM_Image.Raster.N),
                      Element_Index (PNM_Image.Raster.M)
                     )
                    )
                 );
            begin
               Session.Run (Input, Output);
               Output (1).Get_Data (Probability);
            
               Max := Probability'First;
               for J in Probability'Range loop
                  if Probability (Max) < Probability (J) then
                     Max := J;
                  end if;
               end loop;

               Put ("Result:" & Max'Image & " ");
               Put ("Probabilities: ");
               for J in Probability'Range loop
                  Put (Probability (J), 4, 4, 0);
                  Put (" ");
               end loop;
               New_Line;
            end;
         end loop;
         
         Close (File);
         
      end loop;
      
   end;
   
exception
   when Exception_Occurence : NO_MODEL_PROVIDED =>
      Put_Line (Command_Name & ": " & Exception_Message(Exception_Occurence));
      Ada.Command_Line.Set_Exit_Status (No_Model_File_Status);
   
end Ada_Ml;
