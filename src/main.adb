with coursework; use coursework;
with Ada.Text_IO; use Ada.Text_IO;

procedure Main is
   Str: String (1..2);
   Last: Natural;

   task ControlPanel;
   task Climb;
   task GainSpeed;
   task EngineRunning;
   task Descend;
   task SlowDown;

   procedure PrintDashboardControls is
   begin
      Put_Line("------------------------------");
      Put_Line("Boeing 737-800 Dashboard");
      Put_Line("------------------------------");
      Put_Line("Aircraft status: " & p1.Mode'Image);
      Put_Line("Engine: " & p1.Engine'Image);
      Put_Line("Airspeed: " & p1.Airspeed'Image & "mph (550mph MAX)");
      Put_Line("Fuel Level: " & p1.Fuel'Image & "L (26,000L Capacity)");
      Put_Line("Altitude: " & p1.Altitude'Image & "ft (60,000ft Max)");
      Put_Line("Cockpit door: " & p1.Cockpit'Image & " and " & P1.CockpitLock'Image);
      Put_Line("External doors: " & p1.ExternalDoors'Image & " and " & P1.ExternalDoorLocks'Image);
      Put_Line("Landing gear: " & p1.LandingGear'Image);
      if (p1.WarningLights) then
         Put_Line("");
         Put_Line(" * * * * * WARNING * * * * * ");
         Put_Line("");
         Put_Line("");
         Put_Line("");
         Put_Line("");
      elsif (p1.LowfuelWarningLight) then
         Put_Line("");
         Put_Line("");
         Put_Line("");
         Put_Line("");
         Put_Line(" * * * * * LOW FUEL * * * * * ");
         Put_Line("");
      else
         Put_Line("");
         Put_Line("");
         Put_Line("");
         Put_Line("");
         Put_Line("");
         Put_Line("");
      end if;
      Put_Line("------------------------------");
      Put_Line("Boeing 737-800 Control Panel");
      Put_Line("------------------------------");
      Put_Line("(a) - Take off");
      Put_Line("(b) - Switch engine ON");
      Put_Line("(c) - Switch engine OFF");
      Put_Line("(d) - Request fuel");
      Put_Line("(e) - OPEN Cockpit door");
      Put_Line("(f) - SHUT Cockpit door");
      Put_Line("(g) - LOCK Cockpit door");
      Put_Line("(h) - UNLOCK Cockpit door");
      Put_Line("(i) - OPEN External doors");
      Put_Line("(j) - SHUT External doors");
      Put_Line("(k) - LOCK External doors");
      Put_Line("(l) - UNLOCK External doors");
      Put_Line("(m) - Land");
      Put_Line("");
      Put_Line("Enter command:");
   end PrintDashboardControls;

   task body ControlPanel is
   begin
      loop
         PrintDashboardControls;
         Get_Line(Str, Last);
         case (Str(1)) is
         when 'a' => TakeOff;
         when 'b' => EngineOn;
         when 'c' => EngineOff;
         when 'd' =>
            Put_Line("How much fuel would you like onboard? (26,000L Max Capacity)");
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
            when others => abort Climb; abort EngineRunning; exit;
            end case;
         when 'e' => OpenCockpitDoor;
         when 'f' => ShutCockpitDoor;
         when 'g' => LockCockpitDoor;
         when 'h' => UnlockCockpitDoor;
         when 'i' => OpenExternalDoors;
         when 'j' => ShutExternalDoors;
         when 'k' => LockExternalDoors;
         when 'l' => UnlockExternalDoors;
         when 'm' => Land;
         when others => abort Climb; abort EngineRunning; exit;
         end case;
      end loop;
      delay 0.1;
   end ControlPanel;

   task body Climb is
   begin
      loop
         if (P1.Mode = TakingOff and P1.Altitude < LOWALTITUDE
            and P1.Airspeed >= TAKEOFFSPEED)
         then
            IncreaseAltitude;
         elsif (P1.Mode = TakingOff and P1.Altitude >= LOWALTITUDE
                and P1.Altitude < MEDALTITUDE)
         then
            IncreaseAltitude;
            LiftLandingGear;
         elsif (P1.Mode = TakingOff and P1.Altitude >= MEDALTITUDE
                and P1.Altitude < HIGHALTITUDE and P1.Airspeed >= MINCRUISINGSPEED
                and P1.Airspeed <= MAXCRUISINGSPEED)
         then
            InNormalFlight;
         end if;
         delay 0.01;
      end loop;
   end Climb;

   task body GainSpeed is
   begin
      loop
         if (P1.Engine = On and (P1.Mode = TakingOff or P1.Mode = NormalFlight))
         then
            if (P1.Airspeed < MAXCRUISINGSPEED)
            then
               IncreaseAirspeed;
            end if;
         end if;
         delay 0.5;
      end loop;
   end GainSpeed;

   task body EngineRunning is
   begin
      loop
         if (P1.Engine = On)
         then
            BurnFuel;
            PrintDashboardControls;
         end if;
         delay 1.0;
      end loop;
   end EngineRunning;

   task body Descend is
   begin
      loop
         if (P1.Mode = Landing and P1.Altitude > AltitudeRange'First)
         then
            DecreaseAltitude;
            if (P1.Altitude < LOWALTITUDE)
            then
               LowerLandingGear;
            end if;
         elsif (P1.Mode = Landing and P1.Altitude = AltitudeRange'First
                and P1.Airspeed = Speed'First)
         then
            StopPlane;
         end if;
         delay 0.01;
      end loop;
   end Descend;

   task body SlowDown is
   begin
      loop
         if (P1.Mode = Landing and P1.Airspeed > Speed'First)
         then
            DecreaseAirspeed;
         end if;
         delay 0.5;
      end loop;
   end SlowDown;

begin
   null;
end Main;
