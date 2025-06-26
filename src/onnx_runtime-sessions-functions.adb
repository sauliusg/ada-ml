with Interfaces.C; use Interfaces.C;

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
   
end;
