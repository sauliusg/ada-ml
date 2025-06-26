package ONNX_Runtime.Sessions.Functions is
   
   function Session_Input_Count (S : ONNX_Runtime.Sessions.Session)
                                return Integer;

   function Session_Output_Count (S : ONNX_Runtime.Sessions.Session)
                                 return Integer;

end;
