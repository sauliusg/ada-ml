package ONNX_Runtime.Sessions.Functions is
   
   function Session_Input_Count (S : ONNX_Runtime.Sessions.Session)
                                return Integer;

   function Session_Output_Count (S : ONNX_Runtime.Sessions.Session)
                                 return Integer;

   function Session_Input_Name 
     (
      Session : ONNX_Runtime.Sessions.Session;
      Index   : Integer
        -- Index of the input for which the name must be
        --  returned. Index must be between 0 (inclusive) and what
        --  Session_Input_Count returns (exclusive)
     )
     return String;

   function Session_Output_Name 
     (
      Session : ONNX_Runtime.Sessions.Session;
      Index   : Integer
        -- Index of the input for which the name must be
        --  returned. Index must be between 0 (inclusive) and what
        --  Session_Input_Count returns (exclusive)
     )
     return String;

end;
