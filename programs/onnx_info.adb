pragma Ada_2022;

with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Float_Text_IO;   use Ada.Float_Text_IO;
with Ada.Command_Line;    use Ada.Command_Line;
with Ada.Exceptions;      use Ada.Exceptions;

with ONNX_Runtime.Environments;
with ONNX_Runtime.Sessions;
with ONNX_Runtime.Values; use ONNX_Runtime.Values;
with ONNX_Runtime.C_API;  use ONNX_Runtime.C_API;

with ONNX_Runtime.Sessions.Metadata;
use ONNX_Runtime.Sessions.Metadata;

procedure ONNX_Info is

   pragma Style_Checks (Off);
   
   Env : constant ONNX_Runtime.Environments.Environment :=
     ONNX_Runtime.Environments.Create_Environment;

begin
   
   for I in 1 .. Argument_Count loop
      
      declare
         
         Model_File_Name : constant String := Argument (I);
         
         Session : ONNX_Runtime.Sessions.Session :=
           Env.Create_Session (Model => Model_File_Name);
         
         Model_Metadata : OrtModelMetadata;
         
      begin
         Put_Line ("Model:");
         Get_Metadata (Session, Model_Metadata);
      end;
   
   end loop;
   
end ONNX_Info;
