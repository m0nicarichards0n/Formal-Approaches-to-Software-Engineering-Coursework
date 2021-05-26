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
   
   procedure EngineOn is
   begin
      if (MasterInvariant and (P1.Fuel > FuelCapacity'First)
          and (P1.Engine = Off) and (P1.Mode = Stationary)) 
      then
         P1.Engine := On;
      end if;
   end EngineOn;
   
   procedure EngineOff is
   begin
      if (MasterInvariant and (P1.Engine = On)
          and ((P1.Mode = Stationary or P1.Mode = UnderTow)))
      then
         P1.Engine := Off;
      end if;
   end EngineOff;
   
   procedure SetFuel ( f : in FuelCapacity ) is
   begin
      if (MasterInvariant 
          and (((P1.Mode = Stationary) and (f > P1.Fuel)) 
               and ((f > FuelCapacity'First) and (f <= FuelCapacity'Last))))
      then P1.Fuel := f;
      end if;
   end SetFuel;
   
   procedure OpenCockpitDoor is
   begin
      if (MasterInvariant and (P1.Mode = Stationary)
          and ((P1.Cockpit = Shut) and (P1.CockpitLock = Unlocked))) 
      then
         P1.Cockpit := Open;
      end if;
   end OpenCockpitDoor;
   
   procedure ShutCockpitDoor is
   begin
      if (MasterInvariant and (P1.Mode = Stationary) 
          and ((P1.Cockpit = Open) and (P1.CockpitLock = Unlocked))) 
      then
         P1.Cockpit := Shut;
      end if;
   end ShutCockpitDoor;
   
   procedure LockCockpitDoor is
   begin
      if (MasterInvariant and (P1.Mode = Stationary) 
          and ((P1.CockpitLock = Unlocked) and (P1.Cockpit = Shut)))
      then
         P1.CockpitLock := Locked;
      end if;
   end LockCockpitDoor;
   
   procedure UnlockCockpitDoor is
   begin
      if (MasterInvariant and (P1.Mode = Stationary) 
          and ((P1.CockpitLock = Locked) and (P1.Cockpit = Shut)))
      then
         P1.CockpitLock := Unlocked;
      end if;
   end UnlockCockpitDoor;
   
   procedure OpenExternalDoors is
   begin
      if (MasterInvariant and (P1.Mode = Stationary)
          and ((P1.ExternalDoors = Shut) and (P1.ExternalDoorLocks = Unlocked)))
      then
         P1.ExternalDoors := Open;
      end if;
   end OpenExternalDoors;
   
   procedure ShutExternalDoors is
   begin
      if (MasterInvariant and (P1.Mode = Stationary)
          and ((P1.ExternalDoors = Open) and (P1.ExternalDoorLocks = Unlocked)))
      then
         P1.ExternalDoors := Shut;
      end if;
   end ShutExternalDoors;
   
   procedure LockExternalDoors is
   begin
      if (MasterInvariant and (P1.Mode = Stationary)
          and ((P1.ExternalDoorLocks = Unlocked) and (P1.ExternalDoors = Shut)))
      then
         P1.ExternalDoorLocks := Locked;
      end if;
   end LockExternalDoors;
   
   procedure UnlockExternalDoors is
   begin
      if (MasterInvariant and (P1.Mode = Stationary) 
          and ((P1.ExternalDoorLocks = Locked) and (P1.ExternalDoors = Shut)))
      then
         P1.ExternalDoorLocks := Unlocked;
      end if;
   end UnlockExternalDoors;
   
   procedure IncreaseAltitude is
   begin
      if (MasterInvariant and P1.Altitude < AltitudeRange'Last) 
      then
         P1.Altitude := P1.Altitude + 1;
      end if;
   end IncreaseAltitude;
   
   procedure BurnFuel is
   begin
      if (MasterInvariant and P1.Fuel > FuelCapacity'First) 
      then
         P1.Fuel := P1.Fuel - 1;
      end if;
   end BurnFuel;
   
   procedure LiftLandingGear is
   begin
      if (MasterInvariant and (P1.Mode = TakingOff) 
          and (P1.Altitude >= 2000) and (P1.LandingGear = Down))
      then
         P1.LandingGear := Up;
      end if;
   end LiftLandingGear;
   
end coursework;
