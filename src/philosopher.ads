with Ada.Strings;           use Ada.Strings;
with Ada.Strings.Bounded;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Philosopher is
    pragma Elaborate_Body;

    type States is (Eating, Hungry, Thinking);

    function IncPhilosopherId return Integer;
    function GetLastPhilosopherId return Integer;
    procedure SetPhilosopherNum (Num : Integer);
    function GetPhilosopherNum return Integer;

    type Philosopher is record
        Id            : Integer := GetLastPhilosopherId;
        Left_Fork_Id  : Integer := GetLastPhilosopherId;
        Right_Fork_Id : Integer := IncPhilosopherId; 
        State         : States  := Thinking;
    end record;

    function Image (phil : in Philosopher) return String;

private
    Id              : Integer := 1;
    Philosopher_Num : Integer := 5;
end Philosopher;
