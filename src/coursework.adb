with Ada.Text_IO, Ada.Integer_Text_IO, Ada.Float_Text_IO;
use Ada.Text_IO, Ada.Integer_Text_IO, Ada.Float_Text_IO;

package body coursework 
with SPARK_Mode
is

   procedure TakeOff is
   begin
      if (MasterInvariant and TakeOffInvariant
          and ((P1.Cockpit = Shut and P1.CockpitLock = Locked)
               and (P1.ExternalDoors = Shut and P1.ExternalDoorLocks = Locked))
          and ((P1.Fuel >= MINFUEL and P1.Engine = On)
               and (P1.Altitude < LOWALTITUDE and P1.LandingGear = Down))) 
      then
         P1.Mode := TakingOff;
      end if;
   end TakeOff;
   
   procedure IncreaseAltitude is
   begin
      if (MasterInvariant and P1.Altitude < AltitudeRange'Last) then
         P1.Altitude := P1.Altitude + 1;
      end if;
   end IncreaseAltitude;
   
   procedure BurnFuel is
   begin
      if (MasterInvariant and P1.Fuel > FuelCapacity'First) then
         P1.Fuel := P1.Fuel -1;
      end if;
   end BurnFuel;   
   
   
end coursework;
