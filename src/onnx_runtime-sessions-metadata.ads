with ONNX_Runtime.C_API; use ONNX_Runtime.C_API;

package ONNX_Runtime.Sessions.Metadata is

   function Get_Metadata
     (
      Session : ONNX_Runtime.Sessions.Session
     ) return access OrtModelMetadata;
   
   function Producer_Name
     (
      Metadata : access OrtModelMetadata
     ) return String;
   
end ONNX_Runtime.Sessions.Metadata;
