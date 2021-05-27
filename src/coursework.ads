package coursework 
with SPARK_Mode
is
   type OpenShut is (Open, Shut);
   type LockedUnlocked is (Locked, Unlocked);
   type OnOff is (On, Off);
   type UpDown is (Up, Down);
   type FlightMode is (Stationary, UnderTow, TakingOff, NormalFlight, Landing);
   type FuelCapacity is range 0..26000;
   type AltitudeRange is range 0..60000;
   type Speed is range 0..550;
   
   type Plane is record
      Cockpit : OpenShut;
      CockpitLock : LockedUnlocked;
      ExternalDoors : OpenShut;
      ExternalDoorLocks : LockedUnlocked;
      Mode : FlightMode;
      Fuel : FuelCapacity;
      Engine : OnOff;
      Altitude : AltitudeRange;
      LandingGear : UpDown;
      Airspeed : Speed;
      WarningLights : Boolean;
      LowfuelWarningLight : Boolean;
   end record;
   
   P1 : Plane := (Cockpit => Open, CockpitLock => Unlocked,
                  ExternalDoors => Open, ExternalDoorLocks => Unlocked,
                  Mode => Stationary, Fuel => FuelCapacity'First, Engine => Off,
                  Altitude => AltitudeRange'First, LandingGear => Down, 
                  Airspeed => Speed'First, WarningLights => True, 
                  LowfuelWarningLight => True);
   
   -- Warning light comes on when fuel level reaches 5200L
   LOWFUEL : constant := 5200;
   -- Take Off requires at least 13000L of fuel
   MINFUEL : constant := 13000;
   -- Landing Gear lowers below 2000ft
   LOWALTITUDE : constant := 2000;
   -- Normal Flight Mode altitude 10000-30000ft
   MEDALTITUDE : constant := 10000;
   HIGHALTITUDE : constant := 30000;
   -- Normal Flight Mode speed must remain between 400-500 mph
   MINCRUISINGSPEED : constant := 400;
   MAXCRUISINGSPEED : constant := 500;
   -- Plane won't leave the ground until it reaches 150mph
   TAKEOFFSPEED : constant := 150;
   
   function MasterInvariant return Boolean is
     (((P1.Cockpit = Shut and P1.CockpitLock = Locked)
       and (P1.ExternalDoors = Shut and P1.ExternalDoorLocks = Locked))
       or (P1.Mode = Stationary));
      
   function TakeOffInvariant return Boolean is
     (((P1.Fuel >= MINFUEL and P1.Engine = On)
       and (P1.Altitude < LOWALTITUDE and P1.LandingGear = Down))
       or (P1.Mode /= TakingOff));
   
   procedure TakeOff with
     Global => (In_Out => P1),
     Pre => MasterInvariant and TakeOffInvariant 
     and ((P1.Cockpit = Shut and P1.CockpitLock = Locked)
     and (P1.ExternalDoors = Shut and P1.ExternalDoorLocks = Locked))
     and ((P1.Fuel >= MINFUEL and P1.Engine = On)
     and (P1.Altitude < LOWALTITUDE and P1.LandingGear = Down)),
     Post => MasterInvariant and TakeOffInvariant and (P1.Mode = TakingOff);
   
   procedure EngineOn with
     Global => (In_Out => P1),
     Pre => MasterInvariant and (P1.Fuel > FuelCapacity'First)
     and (P1.Engine = Off) and (P1.Mode = Stationary),
     Post => MasterInvariant and (P1.Engine = On);
   
   procedure EngineOff with
     Global => (In_Out => P1),
     Pre => MasterInvariant and (P1.Engine = On) 
     and ((P1.Mode = Stationary or P1.Mode = UnderTow)),
     Post => MasterInvariant and (P1.Engine = Off);
   
   procedure SetFuel ( f : in FuelCapacity ) with
     Global => (In_Out => P1),
     Pre => MasterInvariant and (((P1.Mode = Stationary) and (f > P1.Fuel)) 
     and ((f > FuelCapacity'First) and (f <= FuelCapacity'Last))),
     Post => MasterInvariant and (P1.Fuel > P1.Fuel'Old);
   
   procedure OpenCockpitDoor with
     Global => (In_Out => P1),
     Pre => MasterInvariant and (P1.Mode = Stationary) 
     and ((P1.Cockpit = Shut) and (P1.CockpitLock = Unlocked)),
     Post => MasterInvariant and (P1.Cockpit = Open);
   
   procedure ShutCockpitDoor with
     Global => (In_Out => P1),
     Pre => MasterInvariant and (P1.Mode = Stationary)
     and ((P1.Cockpit = Open) and (P1.CockpitLock = Unlocked)),
     Post => MasterInvariant and (P1.Cockpit = Shut);
   
   procedure LockCockpitDoor with
     Global => (In_Out => P1),
     Pre => MasterInvariant and (P1.Mode = Stationary) 
     and ((P1.CockpitLock = Unlocked) and (P1.Cockpit = Shut)),
     Post => MasterInvariant and (P1.CockpitLock = Locked);
   
   procedure UnlockCockpitDoor with
     Global => (In_Out => P1),
     Pre => MasterInvariant and (P1.Mode = Stationary) 
     and ((P1.CockpitLock = Locked) and (P1.Cockpit = Shut)),
     Post => MasterInvariant and (P1.CockpitLock = Unlocked);
   
   procedure OpenExternalDoors with
     Global => (In_Out => P1),
     Pre => MasterInvariant and (P1.Mode = Stationary) 
     and ((P1.ExternalDoors = Shut) and (P1.ExternalDoorLocks = Unlocked)),
     Post => MasterInvariant and (P1.ExternalDoors = Open);
   
   procedure ShutExternalDoors with
     Global => (In_Out => P1),
     Pre => MasterInvariant and (P1.Mode = Stationary)
     and ((P1.ExternalDoors = Open) and (P1.ExternalDoorLocks = Unlocked)),
     Post => MasterInvariant and (P1.ExternalDoors = Shut);
   
   procedure LockExternalDoors with
     Global => (In_Out => P1),
     Pre => MasterInvariant and (P1.Mode = Stationary) 
     and ((P1.ExternalDoorLocks = Unlocked) and (P1.ExternalDoors = Shut)),
     Post => MasterInvariant and (P1.ExternalDoorLocks = Locked);
   
   procedure UnlockExternalDoors with
     Global => (In_Out => P1),
     Pre => MasterInvariant and (P1.Mode = Stationary) 
     and ((P1.ExternalDoorLocks = Locked) and (P1.ExternalDoors = Shut)),
     Post => MasterInvariant and (P1.ExternalDoorLocks = Unlocked);
   
   procedure IncreaseAltitude with
     Global => (In_Out => P1),
     Pre => MasterInvariant and (P1.Altitude < AltitudeRange'Last),
     Post => MasterInvariant and (P1.Altitude = P1.Altitude'Old + 1);
   
   procedure DecreaseAltitude with
     Global => (In_Out => P1),
     Pre => MasterInvariant and (P1.Altitude > AltitudeRange'First),
     Post => MasterInvariant and (P1.Altitude = P1.Altitude'Old -1);
   
   procedure IncreaseAirspeed with
     Global => (In_Out => P1),
     Pre => MasterInvariant and (P1.Airspeed < Speed'Last)
     and ((P1.Mode = TakingOff) or (P1.Mode = NormalFlight)),
     Post => MasterInvariant and (P1.Airspeed = P1.Airspeed'Old + 1);
   
   procedure DecreaseAirspeed with
     Global => (In_Out => P1),
     Pre => MasterInvariant and (P1.Airspeed > Speed'First)
     and ((P1.Mode = NormalFlight) or (P1.Mode = Landing)),
     Post => MasterInvariant and (P1.Airspeed = P1.Airspeed'Old -1);
   
   procedure BurnFuel with
     Global => (In_out => P1),
     Pre => MasterInvariant and (P1.Fuel > FuelCapacity'First),
     Post => MasterInvariant and (P1.Fuel = P1.Fuel'Old - 1);
   
   procedure LiftLandingGear with
     Global => (In_Out => P1),
     Pre => MasterInvariant and (P1.Mode = TakingOff)
     and (P1.Altitude >= LOWALTITUDE) and (P1.LandingGear = Down),
     Post => MasterInvariant and (P1.LandingGear = Up);
   
   procedure LowerLandingGear with
     Global => (In_Out => P1),
     Pre => MasterInvariant and (P1.Mode = Landing)
     and (P1.Altitude < LOWALTITUDE) and (P1.LandingGear = Up),
     Post => MasterInvariant and (P1.LandingGear = Down);
   
   procedure InNormalFlight with
     Global => (In_Out => P1),
     Pre => MasterInvariant and (P1.Mode = TakingOff)
     and (P1.Altitude >= MEDALTITUDE) and (P1.Altitude < HIGHALTITUDE)
     and (P1.Airspeed >= MINCRUISINGSPEED) and (P1.Airspeed <= MAXCRUISINGSPEED),
     Post => MasterInvariant and (P1.Mode = NormalFlight);
   
   procedure Land with
     Global => (In_Out => P1),
     Pre => MasterInvariant and (P1.Mode = NormalFlight),
     Post => MasterInvariant and (P1.Mode = Landing);
   
   procedure StopPlane with
     Global => (In_Out => P1),
     Pre => MasterInvariant and (P1.Mode = Landing)
     and (P1.Altitude = AltitudeRange'First) and (P1.Airspeed = Speed'First),
     Post => MasterInvariant and (P1.Mode = Stationary);
   
   procedure TowPlane with
     Global => (In_Out => P1),
     Pre => MasterInvariant and (P1.Mode = Stationary) 
     and (P1.Cockpit = Shut) and (P1.CockpitLock = Locked) 
     and (P1.ExternalDoors = Shut) and (P1.ExternalDoorLocks = Locked)
     and (P1.Engine = Off),
     Post => MasterInvariant and (P1.Mode = UnderTow);

end coursework;
