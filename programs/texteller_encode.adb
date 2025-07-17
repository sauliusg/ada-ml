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

procedure Texteller_Encode is

   type PNM_16bin_Pixel is mod 2**16;
   package PNM_16bit_Reader is new PNM_Reader (PNM_16bin_Pixel);
   use PNM_16bit_Reader;

   Env : constant ONNX_Runtime.Environments.Environment :=
     ONNX_Runtime.Environments.Create_Environment;

   Output : ONNX_Runtime.Values.Value_Array (1 .. 1);

   Height : constant := 448;
   Width  : constant := 448;

   function PNM_Raster_To_Array (Image : PNM_Image_Type)
                                return ONNX_Runtime.Values.Float_Array is
      -- shape: [1, 1, Height, Width]
      Image_Size : constant Element_Index := Height * Width;
      Retval : ONNX_Runtime.Values.Float_Array (1 .. Image_Size);
      K : Element_Index := 1;
   begin
      for I in 1 .. Height loop
         for J in 1 .. Width loop
            -- Normalize to [0,1]
            Retval (K) := Float (Image.Raster.Pixels (I, J)) / 255.0;
            K := K + 1;
         end loop;
      end loop;
      return Retval;
   end;

   NO_MODEL_PROVIDED : exception;
   No_Model_File_Status : constant Ada.Command_Line.Exit_Status := 1;
   Model_File_Name : constant String :=
     (if Argument_Count > 0 then Argument (1) else "");

begin

   if Argument_Count = 0 then
      raise NO_MODEL_PROVIDED with
        "A model file (an *.onnx file) name must be provided as the first command line argument";
   end if;

   declare
      Session : ONNX_Runtime.Sessions.Session :=
        Env.Create_Session (Model => Model_File_Name);
      PNM_Image : PNM_Image_Type; 
      File : File_Type;
   begin
      for I in 2 .. Argument_Count loop
         Open (File, In_File, Argument (I));
         Put_Line("Processing file: " & Argument(I));
         while not End_Of_File (File) loop
            Load_Raster (File, PNM_Image);

            declare
               -- Model expects [1, 1, Height, Width]
               Input : constant ONNX_Runtime.Values.Value_Array (1 .. 1) :=
                 (1 => ONNX_Runtime.Values.Create_Tensor
                    (
                     PNM_Raster_To_Array (PNM_Image),
                     (1, 1, Height, Width)
                    )
                 );
            begin
               Session.Run (Input, Output);
               declare
                  Encoded : Float_Array(Element_Index range 1 .. 602880);
               begin
                  Get_Data(Output(1), Encoded);
                  Put_Line("First 10 encoder output floats:");
                  for I in Element_Index range 1 .. 10 loop
                     Put(Encoded(I), Fore => 1, Aft => 6, Exp => 0);
                  end loop;
                  New_Line;
               end;

            end;
         end loop;
         Close (File);
      end loop;
   end;

exception
   when Exception_Occurence : NO_MODEL_PROVIDED =>
      Put_Line (Command_Name & ": " & Exception_Message(Exception_Occurence));
      Ada.Command_Line.Set_Exit_Status (No_Model_File_Status);

end Texteller_Encode;
