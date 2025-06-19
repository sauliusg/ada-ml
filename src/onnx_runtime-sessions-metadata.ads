with ONNX_Runtime.C_API; use ONNX_Runtime.C_API;

package ONNX_Runtime.Sessions.Metadata is

   function Producer_Name (S : Session) return String;
   
   function Graph_Name (S : Session) return String;

private
   
   function Get_Metadata
     (
      Session : ONNX_Runtime.Sessions.Session
     ) return access OrtModelMetadata;
   
end ONNX_Runtime.Sessions.Metadata;
