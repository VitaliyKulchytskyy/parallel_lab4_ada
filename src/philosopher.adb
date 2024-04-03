with Philosopher;
with Ada.Strings.Maps.Constants;
with Ada.Strings.Fixed;

package body Philosopher is
    function Image (phil : in Philosopher) return String is
        S : Unbounded_String;
    begin
        S :=
           Translate
              (To_Unbounded_String (States'Image (phil.State)),
               Ada.Strings.Maps.Constants.Lower_Case_Map);
        return Get_Name(phil) & " is " & To_String (S);
    end Image;

    function Get_Name (phil: in Philosopher) return String is
    begin
        return "P" & Ada.Strings.Fixed.Trim (phil.Id'Img, Ada.Strings.Left);
    end Get_Name;

    function IncPhilosopherId return Integer is
    begin
        if GetPhilosopherNum <= 0 then
            raise Constraint_Error;
        end if;

        Id := Id rem GetPhilosopherNum + 1;
        return Id;
    end IncPhilosopherId;

    function GetLastPhilosopherId return Integer is
    begin
        return Id;
    end GetLastPhilosopherId;

    procedure SetPhilosopherNum (Num : Integer) is
    begin
        Philosopher_Num := Num;
    end SetPhilosopherNum;

    function GetPhilosopherNum return Integer is
    begin
        return Philosopher_Num;
    end GetPhilosopherNum;
end Philosopher;
