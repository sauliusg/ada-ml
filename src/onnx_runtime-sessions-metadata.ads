with ONNX_Runtime.C_API; use ONNX_Runtime.C_API;

package ONNX_Runtime.Sessions.Metadata is

   procedure Get_Metadata
     (
      Session : ONNX_Runtime.Sessions.Session;
      Metadata : out OrtModelMetadata
     );
   
   function Producer_Name (Metadata : aliased OrtModelMetadata) return String;
   
end ONNX_Runtime.Sessions.Metadata;
