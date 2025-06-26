with ONNX_Runtime.C_API;

package body ONNX_Runtime.Allocators is
   
   function Get_Allocator return access ONNX_Runtime.C_API.OrtAllocator is
   begin
      return Result : access ONNX_Runtime.C_API.OrtAllocator do
        Check_Status (API.GetAllocatorWithDefaultOptions (Result'Address));
      end return;
   end Get_Allocator;

end;
