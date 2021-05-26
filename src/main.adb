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
         Put_Line("------------------------------");
         Put_Line("Boeing 737-800 Dashboard");
         Put_Line("------------------------------");
         Put_Line("Aircraft status: " & p1.Mode'Image);
         Put_Line("Engine: " & p1.Engine'Image);
         Put_Line("Fuel Level: " & p1.Fuel'Image & "L (26,020L Capacity)");
         Put_Line("Altitude: " & p1.Altitude'Image & "ft (60,000ft Max)");
         Put_Line("Cockpit door: " & p1.Cockpit'Image & " and " & P1.CockpitLock'Image);
         Put_Line("External doors: " & p1.ExternalDoors'Image & " and " & P1.ExternalDoorLocks'Image);
         Put_Line("Landing gear: " & p1.LandingGear'Image);
         Put_Line("------------------------------");
         Put_Line("Boeing 737-800 Control Panel");
         Put_Line("------------------------------");
         Put_Line("(1) Take off");
         Put_Line("(2) Switch engine ON/OFF");
         Put_Line("(3) Request fuel");
         Put_Line("(4) OPEN Cockpit door");
         Put_Line("(5) SHUT Cockpit door");
         Put_Line("(6) LOCK Cockpit door");
         Put_Line("(7) UNLOCK Cockpit door");

         Get_Line(Str, Last);
         case Str(1) is
         when '1' => TakeOff;
         when '2' => EngineOnOff;
         when '3' => Put_Line("How much fuel would you like onboard?");
            Put_Line("(1) 25%");
            Put_Line("(2) 50%");
            Put_Line("(3) 75%");
            Put_Line("(4) 100%");
            Get_Line(Str, Last);
            case Str(1) is
               when '1' => SetFuel((FuelCapacity'Last/100)*25);
               when '2' => SetFuel((FuelCapacity'Last/100)*50);
               when '3' => SetFuel((FuelCapacity'Last/100)*75);
               when '4' => SetFuel(FuelCapacity'Last);
               when others => exit;
            end case;
         when '4' => OpenCockpitDoor;
         when '5' => ShutCockpitDoor;
         when '6' => LockCockpitDoor;
         when '7' => UnlockCockpitDoor;

         when others => abort Climb; abort EngineRunning; exit;
         end case;
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
