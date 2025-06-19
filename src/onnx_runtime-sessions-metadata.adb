pragma Style_Checks (Off);

package body ONNX_Runtime.Sessions.Metadata is
   
   procedure Get_Metadata
     (
      Session : ONNX_Runtime.Sessions.Session;
      Metadata : out OrtModelMetadata
     ) is
      Return_Status : OrtStatusPtr;
   begin
      Return_Status :=
        API.SessionGetModelMetadata
        (
         Session.Value,
         Metadata'Address
        );
      Check_Status (Return_Status);
   end;
   
end;
   
