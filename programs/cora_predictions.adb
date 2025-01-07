pragma Ada_2022;

with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Integer_Text_IO;      use Ada.Integer_Text_IO;
with Ada.Long_Integer_Text_IO; use Ada.Long_Integer_Text_IO;
with Ada.Float_Text_IO;        use Ada.Float_Text_IO;
with Ada.Command_Line;         use Ada.Command_Line;
with Ada.Exceptions;           use Ada.Exceptions;
with Ada.Characters.Handling;  use Ada.Characters.Handling;
with Interfaces;               use Interfaces;

with Ada.Numerics.Elementary_Functions;
use Ada.Numerics.Elementary_Functions;

with ONNX_Runtime.Environments;
with ONNX_Runtime.Sessions;
with ONNX_Runtime.Values; use ONNX_Runtime.Values;

with Ada.Environment_Variables;

with Ada.Unchecked_Deallocation;

procedure Cora_Predictions is

   pragma Style_Checks (Off);
   
   package OS_Environment renames Ada.Environment_Variables;
   
   -- -------------------------------------------------------------------------
   
   Env : constant ONNX_Runtime.Environments.Environment :=
     ONNX_Runtime.Environments.Create_Environment;

   Output : ONNX_Runtime.Values.Value_Array (1 .. 1);

   Node_Predictions : ONNX_Runtime.Values.Int64_Array (0 .. 7);
   
   -- -------------------------------------------------------------------------
   
   NO_MODEL_PROVIDED : exception;
   NO_DATA_PROVIDED : exception;
   
   No_Model_File_Status : constant Ada.Command_Line.Exit_Status := 1;
   No_Data_File_Status : constant Ada.Command_Line.Exit_Status := 2;
   Program_Error_Status : constant Ada.Command_Line.Exit_Status := 3;
   
   Model_File_Name : constant String :=
     (if Argument_Count > 0 then Argument (1) else "");
     
   -- -------------------------------------------------------------------------
     
   type ONNX_Int64_Array_Access is access ONNX_Runtime.Values.Int64_Array;
   type ONNX_Float_Array_Access is access ONNX_Runtime.Values.Float_Array;
   
   procedure Free is new Ada.Unchecked_Deallocation
     (ONNX_Runtime.Values.Int64_Array, ONNX_Int64_Array_Access);
   
   procedure Free is new Ada.Unchecked_Deallocation
     (ONNX_Runtime.Values.Float_Array, ONNX_Float_Array_Access);
   
   Debug : Boolean := False;
   
   function Load_Data_Table
     (
      File_Name : String;
      Row_Count : out Natural
     )
     return ONNX_Int64_Array_Access
   is
      Rows, Columns : Natural;
      Table_File : File_Type;
   begin
      Open (Table_File, In_File, File_Name);
      
      Get (Table_File, Rows);
      Get (Table_File, Columns);
      
      if Debug then
         Put_Line (Standard_Error, ">>> " & Rows'Image);
         Put_Line (Standard_Error, ">>> " & Columns'Image);
      end if;
      
      declare
         Table_Size : constant Element_Index := Element_Index (Rows * Columns);
         T : constant ONNX_Int64_Array_Access := 
           new ONNX_Runtime.Values.Int64_Array (1 .. Table_Size);
      begin
         for I in 1 .. Table_Size loop
            Get (Table_File, Long_Integer (T (I)));
         end loop;
         
         if Debug then
            Put_Line (Standard_Error, ">>> T(1): " & T(1)'Image);
            for I in T'Range loop
               if T(I) /= 0 then
                  Put_Line (Standard_Error, ">>> First non-zero: " & 
                              T(I)'Image & " at index " & I'Image);
                  exit;
               end if;
            end loop;
            Put_Line (Standard_Error, ">>> T(N): " & T(Table_Size)'Image);
            New_Line (Standard_Error);
         end if;
         
         Close (Table_File);
         
         Row_Count := Rows;
         return T;
      end;
   end;
   
   function Load_Data_Table
     (
      File_Name : String;
      Row_Count : out Natural
     )
     return ONNX_Float_Array_Access
   is
      Rows, Columns : Natural;
      Table_File : File_Type;
   begin
      Open (Table_File, In_File, File_Name);
      
      Get (Table_File, Rows);
      Get (Table_File, Columns);
      
      if Debug then
         Put_Line (Standard_Error, ">>> " & Rows'Image);
         Put_Line (Standard_Error, ">>> " & Columns'Image);
      end if;
      
      declare
         Table_Size : constant Element_Index := Element_Index (Rows * Columns);
         T : constant ONNX_Float_Array_Access := 
           new ONNX_Runtime.Values.Float_Array (1 .. Table_Size);
      begin
         for I in 1 .. Table_Size loop
            Get (Table_File, T (I));
         end loop;
         
         if Debug then
            Put_Line (Standard_Error, ">>> T(1): " & T(1)'Image);
         
            for I in T'Range loop
               if T(I) /= 0.0 then
                  Put_Line (Standard_Error, ">>> First non-zero: " & 
                              T(I)'Image & " at index " & I'Image);
                  exit;
               end if;
            end loop;
            
            Put_Line (Standard_Error, ">>> T(N): " & T(Table_Size)'Image);
            New_Line (Standard_Error);
         end if;
         
         Close (Table_File);
         
         Row_Count := Rows;
         return T;
      end;
   end;
   
   procedure Normalise_Row_Average
     (
      Data : in out ONNX_Runtime.Values.Float_Array;
      Row_Count : in Natural
     ) is
      Row_Start, Row_Length : Positive;
      Sum : Long_Float;
      Denom : Long_Float;
   begin
      pragma Assert (Data'First = 1);
      pragma Assert (Data'Length mod Row_Count = 0);
      Row_Start := 1;
      Row_Length := Data'Length / Row_Count;
      
      while Element_Index (Row_Start) < Data'Last loop
         Sum := 0.0;
         for I in Row_Start .. Row_Start + Row_Length - 1 loop
            Sum := Sum + Long_Float (Data (Element_Index (I)));
         end loop;
         Denom := Sum;
         for I in Row_Start .. Row_Start + Row_Length - 1 loop
            Data (Element_Index (I)) :=
              Float (Long_Float (Data (Element_Index (I))) / Denom);
         end loop;
         Row_Start := Row_Start + Row_Length;
      end loop;
   end;
   
begin
   if Ada.Environment_Variables.Exists ("CORA_PREDICTIONS_DEBUG") and then
     (Ada.Environment_Variables.Value ("CORA_PREDICTIONS_DEBUG") = "1" or else
        To_Lower (Ada.Environment_Variables.Value ("CORA_PREDICTIONS_DEBUG"))
        = "true")
   then
      Debug := True;
   end if;

   if Argument_Count = 0 then
      raise NO_MODEL_PROVIDED with
        "a model file (an *.onnx file) name must be provided " &
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
      
      Node_Count : Natural;
      Node_Tensor : ONNX_Float_Array_Access :=
        Load_Data_Table (Argument (2), Node_Count);
      
      Edge_Count : Natural;
      Edge_Tensor : ONNX_Int64_Array_Access :=
        Load_Data_Table (Argument (3), Edge_Count);
   begin
      
      if Debug then
         Put_Line (Standard_Error, ">>> Node_Count: " & Node_Count'Image);
         Put_Line (Standard_Error, ">>> Node Features: " & 
                     Natural'Image (Node_Tensor.all'Length / Node_Count));
         New_Line (Standard_Error);
      
         Put_Line (Standard_Error, ">>> Edge_Count: " & Edge_Count'Image);
         Put_Line (Standard_Error, ">>> Edge Features: " &
                     Natural'Image (Edge_Tensor.all'Length / Edge_Count));
         New_Line (Standard_Error);
      end if;
      
      Normalise_Row_Average (Node_Tensor.all, Node_Count);
      
      declare
         Input : constant ONNX_Runtime.Values.Value_Array (1 .. 2) :=
           [1 => ONNX_Runtime.Values.Create_Tensor
              (
               Node_Tensor.all,
               [
                 Element_Index (Node_Count),
                 Element_Index (Node_Tensor.all'Length / Node_Count)
               ]
              ),
            2 => ONNX_Runtime.Values.Create_Tensor
              (
               Edge_Tensor.all,
               [
                 Element_Index (Edge_Count),
                 Element_Index (Edge_Tensor.all'Length / Edge_Count)
               ]
              )
           ];
      begin
         
         Session.Run (Input, Output);
         
         declare
            N_Classes : constant Element_Index := 7;
            N_Values : constant Element_Index :=
              Element_Index(Node_Count) * N_Classes;
            
            Classes : ONNX_Runtime.Values.Float_Array (1 .. N_Values);
            Matrix : array (1 .. N_Classes, 1 .. Node_Count) of Float;
            
            K : Element_Index;
            C : Element_Index range 1 .. 7;
            V : Float;
         begin
            Output (1).Get_Data (Classes);
            
            Put ("Idx"); Put (ASCII.HT);
            Put ("Class1"); Put (ASCII.HT);
            Put ("Class2"); Put (ASCII.HT);
            Put ("Class3"); Put (ASCII.HT);
            Put ("Class4"); Put (ASCII.HT);
            Put ("Class5"); Put (ASCII.HT);
            Put ("Class6"); Put (ASCII.HT);
            Put ("Class7"); Put (ASCII.HT);
            Put ("Class"); New_Line;
            
            K := 1;
            for I in 1 .. N_Classes loop
               for J in 1 .. Node_Count loop
                  Matrix (I, J) := Classes (K);
                  K := K + 1;
               end loop;
            end loop;
            
            for J in 1 .. Node_Count loop
               Put (J-1,0);
               C := 1;
               V := Matrix (C, J);
               for I in 1 .. N_Classes loop
                  Put (ASCII.HT);
                  Put (Matrix (I, J),0,3,0);
                  if V < Matrix (I, J) then
                     V := Matrix (I, J);
                     C := I;
                  end if;
               end loop;
               Put (ASCII.HT);
               Put (Integer (C), 0);
               New_Line;
            end loop;
         end;
      end;
      
      Free (Node_Tensor);
      Free (Edge_Tensor);
      
   end;
   
exception
   when 
     Exception_Occurence : NO_MODEL_PROVIDED =>
      Put_Line (Command_Name & ": " & Exception_Message(Exception_Occurence));
      Ada.Command_Line.Set_Exit_Status (No_Model_File_Status);
      
   when
     Exception_Occurence : NO_DATA_PROVIDED =>
      Put_Line (Command_Name & ": " & Exception_Message(Exception_Occurence));
      Ada.Command_Line.Set_Exit_Status (No_Data_File_Status);
   
   -- when
   --   Exception_Occurence : PROGRAM_ERROR =>
   --    Put_Line (Command_Name & ": " & "PROGRAM_ERROR, """ &
   --                Exception_Message(Exception_Occurence) & """");
   --    Ada.Command_Line.Set_Exit_Status (Program_Error_Status);
   
end Cora_Predictions;
