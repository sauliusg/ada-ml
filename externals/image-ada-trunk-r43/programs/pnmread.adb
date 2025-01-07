with Text_IO;             use Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Command_Line;    use Ada.Command_Line;
with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;

with PNM_Reader;

procedure PNMRead is
   
   -- subtype Pixel_Type16 is Short_Integer;
   type Pixel_Type16 is mod 2**16;
   
   package PNM_Reader16 is new PNM_Reader (Pixel_Type16);
   use PNM_Reader16;
   
   subtype Pixel_Array_Type16 is PNM_Reader16.Pixel_Array_Type;
   
begin
   
   for I in 1 .. Argument_Count loop
      declare
         Image : PNM_Image_Type;
         Log_Max : Float;
         N_Digits : Integer;
         Input_File : File_Type;
      begin
         Open (Input_File, In_File, Ada.Command_Line.Argument (I));
         while not End_Of_File (Input_File) loop
            Load_Raster (Input_File, Image);
            Put_Line ("P2");
            Put_Line ("# " & Image.Format'Image (1 .. 2));
            Put (Image.Raster.M, 0);
            Put (" ");
            Put (Image.Raster.N, 0);
            New_Line;
            Put (Image.MaxVal, 0);
            New_Line;
         
            Log_Max := Log (Float (Image.MaxVal), Base => 10.0);
            N_Digits := Integer (Log_Max) + 1;
            -- Put_Line (">>> " & N_Digits'Image);

            for I in Image.Raster.Pixels'Range (1) loop
               for J in Image.Raster.Pixels'Range (2) loop
                  Put (Integer (Image.Raster.Pixels (I, J)), N_Digits + 1);
               end loop;
               New_Line;
            end loop;
         end loop;
         Close (Input_File);
      end;      
      if I < Argument_Count then
         New_Line;
      end if;
   end loop;
   
end PNMRead;
