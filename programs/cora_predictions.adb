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

with Ada.Environment_Variables;

procedure Cora_Predictions is

   pragma Style_Checks (Off);
   
   package OS_Environment renames Ada.Environment_Variables;
   
   -- -------------------------------------------------------------------------
   
   Env : constant ONNX_Runtime.Environments.Environment :=
     ONNX_Runtime.Environments.Create_Environment;

   Output : ONNX_Runtime.Values.Value_Array (1 .. 1);

   Node_Predictions : ONNX_Runtime.Values.Float_Array (0 .. 7);
   
   -- -------------------------------------------------------------------------
   
   NO_MODEL_PROVIDED : exception;
   NO_DATA_PROVIDED : exception;
   
   No_Model_File_Status : constant Ada.Command_Line.Exit_Status := 1;
   
   Model_File_Name : constant String :=
     (if Argument_Count > 0 then Argument (1) else "");
     
   -- -------------------------------------------------------------------------
     
begin
   
   if Argument_Count = 0 then
      raise NO_MODEL_PROVIDED with
        "a model file name must be provided " &
        "as the first command line argument";
   end if;
   
   if Argument_Count < 3 then
      raise NO_DATA_PROVIDED with
        "two data file names, for files with nodes and edges " &
        "must be provided " &
        "as the second and the third command line argument, " &
        "e.g. ""nodes.dat"" and ""edges.dat""";
   end if;
   
   declare
      Session : ONNX_Runtime.Sessions.Session :=
        Env.Create_Session (Model => Model_File_Name);
      Node_File, Edge_File : File_Type;
      -- Node_Tensor, Edge_Tensor : ONNX_Runtime.Values.Float_Array;
      Node_Tensor, Edge_Tensor : ONNX_Runtime.Values.Float_Array(1..100);
   begin

      Open (Node_File, In_File, Argument (2));
      Open (Edge_File, In_File, Argument (3));
         
      declare
         Input : constant ONNX_Runtime.Values.Value_Array (1 .. 2) :=
           [1 => ONNX_Runtime.Values.Create_Tensor
              (
               Node_Tensor,
               [1, 1, 100, 1000]
              ),
            2 => ONNX_Runtime.Values.Create_Tensor
              (
               Edge_Tensor,
               [1, 1, 200, 2000]
              )
           ];
      begin
         
         Session.Run (Input, Output);
         
      end;
      
      Close (Node_File);
      Close (Edge_File);
         
   end;
   
exception
   when Exception_Occurence : NO_MODEL_PROVIDED | NO_DATA_PROVIDED =>
      Put_Line (Command_Name & ": " & Exception_Message(Exception_Occurence));
      Ada.Command_Line.Set_Exit_Status (No_Model_File_Status);
   
end Cora_Predictions;
