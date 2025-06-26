pragma Style_Checks (Off);

with Interfaces.C.Strings;
with System;

with ONNX_Runtime.Allocators; use ONNX_Runtime.Allocators;

package body ONNX_Runtime.Sessions.Metadata is
   
   function Producer_Name (S : Session) return String
   is

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
      
      API.ReleaseModelMetadata (Model_Metadata);
      
      return Result : String := Interfaces.C.Strings.Value (Name_Chars) do
        Allocator.Free (Allocator, Cast (Name_Chars));
      end return;
      
   end Producer_Name;
   
   function Graph_Name (S : Session) return String
   is
      
      Allocator  : constant access ONNX_Runtime.C_API.OrtAllocator :=
        Get_Allocator;
      
      Name_Chars : Interfaces.C.Strings.Chars_Ptr;
      
      Model_Metadata : aliased access OrtModelMetadata;
         
   begin
      Model_Metadata := Get_Metadata (S);
      
      Check_Status 
        (API.ModelMetadataGetGraphName
           (
            Model_Metadata,
            Allocator,
            Name_Chars'Address
           )
        );
      
      API.ReleaseModelMetadata (Model_Metadata);
      
      return Result : String := Interfaces.C.Strings.Value (Name_Chars) do
        Allocator.Free (Allocator, Cast (Name_Chars));
      end return;
      
   end Graph_Name; 

   -- private subroutines:
   
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
   
