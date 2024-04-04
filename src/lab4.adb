with Philosopher;           use Philosopher;
with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Exceptions;        use Ada.Exceptions;
with GNAT.Semaphores;       use GNAT.Semaphores;
with Ada.Containers;        use Ada.Containers;

procedure Lab4 is
    Forks       :
       array
          (1 ..
                 Philosopher.GetPhilosopherNum) of Counting_Semaphore
          (1, Default_Ceiling);
    -- Waiter : Counting_Semaphore(2, Default_Ceiling);
    Phil_States : array (1 .. 5) of Philosopher.States;

    protected State is
        procedure Set (Id : Integer; S : Philosopher.States);
        function Get (Id : Integer) return Philosopher.States;
    end State;

    protected body State is
        procedure Set (Id : Integer; S : Philosopher.States) is
        begin
            Phil_States (Id) := S;
        end Set;

        function Get (Id : Integer) return Philosopher.States is
        begin
            return Phil_States (Id);
        end Get;
    end State;

    function Left (Id : Integer) return Integer is
    begin
        return (Id) rem Philosopher.GetPhilosopherNum;
    end Left;
    pragma Inline (Left);

    function Right (Id : Integer) return Integer is
    begin
        return (Id rem Philosopher.GetPhilosopherNum) + 1;
    end Right;
    pragma Inline (Right);

    procedure Test (Id : Integer) is
    begin
        if  State.Get (Id) = Hungry and 
            State.Get (Left (Id)) /= Eating and
            State.Get (Right (Id)) /= Eating
        then
            State.Set (Id, Eating);
            Put_Line (Id'Image & " eating");
            Forks (Id).Release;
            Forks (Right (Id)).Release;
        end if;
    end Test;

    procedure Philosopher_Thinking (Id : Integer; Delay_Sec : Duration := 0.2)
    is
    begin
        State.Set (Id, Thinking);
        -- Put_Line (Philosopher.Image(Phil));
        Put_Line (Id'Image & " thinking");
        delay Delay_Sec;
    end Philosopher_Thinking;

    procedure Philosopher_Take_Fork (Id : Integer) is
    begin
        State.Set (Id, Hungry);
        Test (Id);
        Forks (Id).Seize;
        Forks (Right (Id)).Seize;
    end Philosopher_Take_Fork;

    procedure Philosopher_Put_Fork (Id : Integer) is
    begin
        State.Set (Id, Thinking);
        Test (Left (Id));
        Test (Right (Id));
    end Philosopher_Put_Fork;

    procedure Philiospher_Diner (Id : Integer; Eating_Sec : Duration := 0.2) is
    begin
        Philosopher_Thinking (Id);
        Philosopher_Take_Fork (Id);
        delay Eating_Sec;
        Philosopher_Put_Fork (Id);
    end Philiospher_Diner;

    task type Philosopher_Parallel (Id : Integer);
    task body Philosopher_Parallel is
    begin
        for Life_Cycle in 1 .. 10 loop
            Philiospher_Diner (Id, 0.5);
        end loop;
        Put_Line (Id'Image & " is leaving");
    end Philosopher_Parallel;

    Ph1 : Philosopher_Parallel (1);
    Ph2 : Philosopher_Parallel (2);
    Ph3 : Philosopher_Parallel (3);
    Ph4 : Philosopher_Parallel (4);
    Ph5 : Philosopher_Parallel (5);

begin
    null;

exception
    when e : Constraint_Error =>
        Put_Line
           ("[!] Divide by zero exception. Set another number of Philosophers (> 0).");

end Lab4;
