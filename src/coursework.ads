package coursework 
with SPARK_Mode
is
   type DoorPosition is (Open, Shut);
   type LockState is (Locked, Unlocked);
   type FlightMode is (Stationary, UnderTow, TakingOff, NormalFlight, Landing);
   type FuelLevel is range 1..26020;
   
   Cockpit : DoorPosition;
   ExternalDoors : DoorPosition;
   CockpitLock : LockState;
   ExternalDoorLocks : LockState;
   Plane : FlightMode;
   FuelOnboard : FuelLevel;
   
   MINFUEL : constant := 13986;
   
   function Invariant return Boolean is
     (((Cockpit = Shut and CockpitLock = Locked)
      and (ExternalDoors = Shut and ExternalDoorLocks = Locked)
      and (FuelOnboard >= MINFUEL)) or (Plane = Stationary));
   
   procedure TakeOff with
     Global => (Proof_In => (Cockpit,CockpitLock,ExternalDoors,ExternalDoorLocks, FuelOnboard),
                In_Out => Plane),
     Pre => Invariant and ((Cockpit = Shut and CockpitLock = Locked)
                      and (ExternalDoors = Shut and ExternalDoorLocks = Locked)
                      and (FuelOnboard >= MINFUEL)),
     Post => Invariant and (Plane = TakingOff);

end coursework;
