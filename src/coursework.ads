package coursework 
with SPARK_Mode
is
   type DoorPosition is (Open, Shut);
   type LockState is (Locked, Unlocked);
   type EngineState is (On, Off);
   type LandingGearState is (Up, Down);
   type FlightMode is (Stationary, UnderTow, TakingOff, NormalFlight, Landing);
   type FuelLevel is range 1..26020;
   type Altitude is range 1..60000;
   
   Cockpit : DoorPosition;
   ExternalDoors : DoorPosition;
   CockpitLock : LockState;
   ExternalDoorLocks : LockState;
   Plane : FlightMode;
   FuelOnboard : FuelLevel;
   Engine: EngineState;
   CurrentAltitude : Altitude;
   LandingGear : LandingGearState;
   
   MINFUEL : constant := 13986;
   LOWALTITUDE : constant := 2000;
   
   function MasterInvariant return Boolean is
     (((Cockpit = Shut and CockpitLock = Locked)
       and (ExternalDoors = Shut and ExternalDoorLocks = Locked))
       or (Plane = Stationary));
      
   function TakeOffInvariant return Boolean is
     (((FuelOnboard >= MINFUEL and Engine = On)
       and (CurrentAltitude < LOWALTITUDE and LandingGear = Down))
       or (Plane /= TakingOff));
   
   procedure TakeOff with
     Global => (Proof_In => (Cockpit,CockpitLock,ExternalDoors,ExternalDoorLocks, 
                             FuelOnboard, Engine, CurrentAltitude, LandingGear),
                In_Out => Plane),
     Pre => MasterInvariant and TakeOffInvariant 
     and ((Cockpit = Shut and CockpitLock = Locked)
     and (ExternalDoors = Shut and ExternalDoorLocks = Locked))
     and ((FuelOnboard >= MINFUEL and Engine = On)
     and (CurrentAltitude < LOWALTITUDE and LandingGear = Down)),
     Post => MasterInvariant and TakeOffInvariant and (Plane = TakingOff);
 
   

end coursework;
