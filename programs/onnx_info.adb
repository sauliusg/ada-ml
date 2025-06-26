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
use  ONNX_Runtime.Sessions.Metadata;

with ONNX_Runtime.Sessions.Functions;
use  ONNX_Runtime.Sessions.Functions;

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
         
      begin
         Put_Line ("Model File:" & ASCII.HT &
                     Model_File_Name);
         
         Put_Line ("Producer Name:" & ASCII.HT &
                     Producer_Name (Session));
         
         Put_Line ("Graph Name:" & ASCII.HT &
                     Graph_Name (Session));
         
         Put ("Input Count:" & ASCII.HT);
         Put (Session_Input_Count (Session));
         New_Line;
         
         New_Line;
      end;
   
   end loop;
   
end ONNX_Info;
