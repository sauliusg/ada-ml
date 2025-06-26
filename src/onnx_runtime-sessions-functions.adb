with Interfaces.C.Strings; use Interfaces.C.Strings;

with ONNX_Runtime.Allocators; use ONNX_Runtime.Allocators;

package body ONNX_Runtime.Sessions.Functions is
   
   function Session_Input_Count (S : ONNX_Runtime.Sessions.Session)
                                return Integer is
      N_Inputs : aliased Interfaces.C.Size_T;
   begin
      Check_Status 
        (API.SessionGetInputCount
           (
            S.Value,
            N_Inputs'Access
           )
        );
      return Integer (N_Inputs);
   end;
   
   function Session_Output_Count (S : ONNX_Runtime.Sessions.Session)
                                return Integer is
      N_Outputs : aliased Interfaces.C.Size_T;
   begin
      Check_Status 
        (API.SessionGetOutputCount
           (
            S.Value,
            N_Outputs'Access
           )
        );
      return Integer (N_Outputs);
   end;
   
   function Session_Input_Name 
     (
      Session : ONNX_Runtime.Sessions.Session;
      Index : Integer 
        -- Index of the input for which the name must be
        --  returned. Index must be between 0 (inclusive) and what
        --  Session_Input_Count returns (exclusive)
     )
     return String is
      
      Allocator  : constant access ONNX_Runtime.C_API.OrtAllocator :=
        Get_Allocator;
      
      -- The 'Name_Chars' value will be set to a null terminated UTF-8
      --  encoded string allocated using `allocator`. Must be freed
      --  using `allocator`:
      
      Name_Chars : Interfaces.C.Strings.Chars_Ptr;
      
   begin
      Check_Status
        (API.SessionGetInputName
           (
            Session.Value,
            Interfaces.C.Size_T (Index),
            Allocator,
            Name_Chars'Address
           )
        );
      
      return Result : String := Interfaces.C.Strings.Value (Name_Chars) do
        Allocator.Free (Allocator, Cast (Name_Chars));
      end return;
   end;
   
end;
