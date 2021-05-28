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
         P1.WarningLights := False;
      else
         P1.WarningLights := True;
      end if;
   end TakeOff;
   
   procedure EngineOn is
   begin
      if (MasterInvariant and (P1.Fuel > FuelCapacity'First)
          and (P1.Engine = Off) and (P1.Mode = Stationary)) 
      then
         P1.Engine := On;
         P1.WarningLights := False;
      else
         P1.WarningLights := True;
      end if;
   end EngineOn;
   
   procedure EngineOff is
   begin
      if (MasterInvariant and (P1.Engine = On)
          and ((P1.Mode = Stationary or P1.Mode = UnderTow)))
      then
         P1.Engine := Off;
         P1.WarningLights := False;
      else
         P1.WarningLights := True;
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
         P1.WarningLights := False;
      else
         P1.WarningLights := True;
      end if;
   end OpenCockpitDoor;
   
   procedure ShutCockpitDoor is
   begin
      if (MasterInvariant and (P1.Mode = Stationary) 
          and ((P1.Cockpit = Open) and (P1.CockpitLock = Unlocked))) 
      then
         P1.Cockpit := Shut;
         P1.WarningLights := False;
      else
         P1.WarningLights := True;
      end if;
   end ShutCockpitDoor;
   
   procedure LockCockpitDoor is
   begin
      if (MasterInvariant and (P1.Mode = Stationary) 
          and ((P1.CockpitLock = Unlocked) and (P1.Cockpit = Shut)))
      then
         P1.CockpitLock := Locked;
         P1.WarningLights := False;
      else
         P1.WarningLights := True;
      end if;
   end LockCockpitDoor;
   
   procedure UnlockCockpitDoor is
   begin
      if (MasterInvariant and (P1.Mode = Stationary) 
          and ((P1.CockpitLock = Locked) and (P1.Cockpit = Shut)))
      then
         P1.CockpitLock := Unlocked;
         P1.WarningLights := False;
      else
         P1.WarningLights := True;
      end if;
   end UnlockCockpitDoor;
   
   procedure OpenExternalDoors is
   begin
      if (MasterInvariant and (P1.Mode = Stationary)
          and ((P1.ExternalDoors = Shut) and (P1.ExternalDoorLocks = Unlocked)))
      then
         P1.ExternalDoors := Open;
         P1.WarningLights := False;
      else
         P1.WarningLights := True;
      end if;
   end OpenExternalDoors;
   
   procedure ShutExternalDoors is
   begin
      if (MasterInvariant and (P1.Mode = Stationary)
          and ((P1.ExternalDoors = Open) and (P1.ExternalDoorLocks = Unlocked)))
      then
         P1.ExternalDoors := Shut;
         P1.WarningLights := False;
      else
         P1.WarningLights := True;
      end if;
   end ShutExternalDoors;
   
   procedure LockExternalDoors is
   begin
      if (MasterInvariant and (P1.Mode = Stationary)
          and ((P1.ExternalDoorLocks = Unlocked) and (P1.ExternalDoors = Shut)))
      then
         P1.ExternalDoorLocks := Locked;
         P1.WarningLights := False;
      else
         P1.WarningLights := True;
      end if;
   end LockExternalDoors;
   
   procedure UnlockExternalDoors is
   begin
      if (MasterInvariant and (P1.Mode = Stationary) 
          and ((P1.ExternalDoorLocks = Locked) and (P1.ExternalDoors = Shut)))
      then
         P1.ExternalDoorLocks := Unlocked;
         P1.WarningLights := False;
      else
         P1.WarningLights := True;
      end if;
   end UnlockExternalDoors;
   
   procedure IncreaseAltitude is
   begin
      if (MasterInvariant and P1.Altitude < AltitudeRange'Last) 
      then
         P1.Altitude := P1.Altitude + 1;
      end if;
   end IncreaseAltitude;
   
   procedure DecreaseAltitude is
   begin
      if (MasterInvariant and P1.Altitude > AltitudeRange'First)
      then
         P1.Altitude := P1.Altitude - 1;
      end if;
   end DecreaseAltitude;
   
   procedure IncreaseAirspeed is
   begin
      if (MasterInvariant and (P1.Airspeed < Speed'Last)
          and ((P1.Mode = TakingOff) or (P1.Mode = NormalFlight)))
      then
         P1.Airspeed := P1.Airspeed + 1;
      end if;
   end IncreaseAirspeed;
   
   procedure DecreaseAirspeed is
   begin
      if (MasterInvariant and (P1.Airspeed > Speed'First)
          and ((P1.Mode = NormalFlight) or (P1.Mode = Landing)))
      then
         P1.Airspeed := P1.Airspeed - 1;
      end if;
   end DecreaseAirspeed;
   
   procedure BurnFuel is
   begin
      if (MasterInvariant and P1.Fuel > FuelCapacity'First) 
      then
         P1.Fuel := P1.Fuel - 1;
         P1.LowfuelWarningLight := False;
         if (P1.Fuel <= LOWFUEL)
         then
            P1.LowfuelWarningLight := True;
         end if;
      end if;
   end BurnFuel;
   
   procedure LiftLandingGear is
   begin
      if (MasterInvariant and (P1.Mode = TakingOff) 
          and (P1.Altitude >= LOWALTITUDE) and (P1.LandingGear = Down))
      then
         P1.LandingGear := Up;
      end if;
   end LiftLandingGear;
   
   procedure LowerLandingGear is
   begin
      if (MasterInvariant and (P1.Mode = Landing)
          and (P1.Altitude < LOWALTITUDE) and (P1.LandingGear = Up))
      then
         P1.LandingGear := Down;
      end if;
   end LowerLandingGear;
   
   procedure InNormalFlight is
   begin
      if (MasterInvariant and (P1.Mode = TakingOff)
          and (P1.Altitude >= MEDALTITUDE) and (P1.Altitude < HIGHALTITUDE)
          and (P1.Airspeed >= MINCRUISINGSPEED) and (P1.Airspeed <= MAXCRUISINGSPEED))
      then
         P1.Mode := NormalFlight;
      end if;
   end InNormalFlight;
   
   procedure Land is
   begin
      if (MasterInvariant and (P1.Mode = NormalFlight))
      then
         P1.Mode := Landing;
         P1.WarningLights := False;
      else
         P1.WarningLights := True;
      end if;
   end Land;
   
   procedure StopPlane is
   begin
      if (MasterInvariant and (P1.Mode = Landing)
          and (P1.Altitude = AltitudeRange'First) and (P1.Airspeed = Speed'First))
      then
         P1.Mode := Stationary;
      end if;
   end StopPlane;
   
   procedure TowPlane is
   begin
      if (MasterInvariant and (P1.Mode = Stationary)
          and (P1.Cockpit = Shut) and (P1.CockpitLock = Locked)
          and (P1.ExternalDoors = Shut) and (P1.ExternalDoorLocks = Locked)
          and (P1.Engine = Off))
      then
         P1.Mode := UnderTow;
         P1.WarningLights := False;
      else
         P1.WarningLights := True;
      end if;
   end TowPlane;
   
end coursework;
