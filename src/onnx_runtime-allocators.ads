with Interfaces.C.Strings;
with System;

with Ada.Unchecked_Conversion;

with ONNX_Runtime.C_API;

package ONNX_Runtime.Allocators is
   
   function Cast is new Ada.Unchecked_Conversion
     (Interfaces.C.Strings.chars_ptr, System.Address);
   
   function Get_Allocator return access ONNX_Runtime.C_API.OrtAllocator;

end;
