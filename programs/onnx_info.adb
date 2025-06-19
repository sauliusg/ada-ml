pragma Ada_2022;

with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Float_Text_IO;   use Ada.Float_Text_IO;
with Ada.Command_Line;    use Ada.Command_Line;
with Ada.Exceptions;      use Ada.Exceptions;

with ONNX_Runtime.Environments;
with ONNX_Runtime.Sessions;
with ONNX_Runtime.Values; use ONNX_Runtime.Values;

procedure MNIST_Predictions is

   pragma Style_Checks (Off);
   
   Env : constant ONNX_Runtime.Environments.Environment :=
     ONNX_Runtime.Environments.Create_Environment;

begin
   
   for I in 1 .. Argument_Count loop
      
      declare
         
         Model_File_Name : constant String := Argument (I);
         
         Session : ONNX_Runtime.Sessions.Session :=
           Env.Create_Session (Model => Model_File_Name);
         
      begin
         Put_Line ("Model:");
      end;
   
   end loop;
   
end MNIST_Predictions;
