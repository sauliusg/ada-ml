pragma Style_Checks (Off);

with Ada.Unchecked_Conversion;
with Interfaces.C.Strings;
with System;

package body ONNX_Runtime.Sessions.Metadata is
   
   function Producer_Name (S : Session) return String
   is
      function Cast is new Ada.Unchecked_Conversion
        (Interfaces.C.Strings.chars_ptr, System.Address);

      function Get_Allocator return access ONNX_Runtime.C_API.OrtAllocator is
      begin
         return Result : access ONNX_Runtime.C_API.OrtAllocator do
            Check_Status (API.GetAllocatorWithDefaultOptions (Result'Address));
         end return;
      end Get_Allocator;

      Allocator  : constant access ONNX_Runtime.C_API.OrtAllocator :=
        Get_Allocator;
      
      Name_Chars : Interfaces.C.Strings.Chars_Ptr;
      
      Model_Metadata : aliased access OrtModelMetadata;
         
   begin
      Model_Metadata := Get_Metadata (S);
         
      Check_Status 
        (API.ModelMetadataGetProducerName
           (
            Model_Metadata,
            Allocator,
            Name_Chars'Address
           )
        );
      
      return Interfaces.C.Strings.Value (Name_Chars);
   end;
   
   function Get_Metadata
     (
      Session : ONNX_Runtime.Sessions.Session
     ) return access OrtModelMetadata
   is
      Local_Metadata_Access : aliased access OrtModelMetadata;
   begin
      Check_Status 
        (API.SessionGetModelMetadata
           (
            Session.Value,
            Local_Metadata_Access'Address
           )
        );
      return Local_Metadata_Access;
   end;
   
end;
   
