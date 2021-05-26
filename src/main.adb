with coursework; use coursework;
with Ada.Text_IO; use Ada.Text_IO;

procedure Main is
   Str: String (1..2);
   Last: Natural;

   task ControlPanel;
   task Climb;
   task EngineRunning;

   task body ControlPanel is
   begin
      loop
         if (P1.Mode = Stationary) then
            Put_Line("------------------------------");
            Put_Line("Boeing 737-800 Control Panel");
            Put_Line("------------------------------");
            Put_Line("(1) Take off");
            Get_Line(Str, Last);
            case Str(1) is
               when '1' => TakeOff;
               when others => abort Climb; abort EngineRunning; exit;
            end case;
         end if;
      end loop;
      delay 0.1;
   end ControlPanel;

   task body Climb is
   begin
      loop
         if (P1.Mode = TakingOff and P1.Altitude < 2000)
         then IncreaseAltitude;
            Put_Line("Current altitude is: " & P1.Altitude'Image & "ft");
         end if;
         delay 1.0;
      end loop;
   end Climb;

   task body EngineRunning is
   begin
      loop
         if (P1.Engine = On)
         then BurnFuel;
            Put_Line("Current fuel level is: " & P1.Fuel'Image & "L");
         end if;
         delay 1.0;
      end loop;
   end EngineRunning;

begin
   null;
end Main;
