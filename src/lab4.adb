with Philosopher;           use Philosopher;
with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Exceptions;        use Ada.Exceptions;
with GNAT.Semaphores;       use GNAT.Semaphores;
with Ada.Containers;        use Ada.Containers;

procedure Lab4 is
    Forks :
       array
          (1 .. Philosopher.GetPhilosopherNum) of Counting_Semaphore (1, Default_Ceiling);
    Waiter : Counting_Semaphore(0, Default_Ceiling); 
    Phil_States : array (1 .. 5) of Philosopher.States;

    protected State is
        procedure Set (Id: Integer; S: Philosopher.States);
        function Get (Id: Integer) return Philosopher.States;
    end State;

    protected body State is 
        procedure Set (Id: Integer; S: Philosopher.States) is
        begin
            Phil_States(Id) := S;
        end Set;

        function Get (Id: Integer) return Philosopher.States is
        begin
            return Phil_States(Id);
        end Get;
    end State;

    function Left (Id: Integer) return Integer is
    begin
        return (Id) rem Philosopher.GetPhilosopherNum;
    end Left;
    pragma Inline (Left);

    function Right (Id: Integer) return Integer is
    begin
        return (Id rem Philosopher.GetPhilosopherNum) + 1;
    end Right;
    pragma Inline (Right);

    procedure Test (Phil : out Philosopher.Philosopher) is
        Id : Integer := Phil.Id;
    begin
        if State.Get(Id) = Hungry and State.Get(Left(Id)) /= Eating and State.Get(Right(Id)) /= Eating then
            State.Set(Id, Eating);
            Forks(Id).Release;
        end if;
    end Test;

    procedure Philosopher_Thinking (Phil: out Philosopher.Philosopher; Delay_Sec: Duration := 0.5) is
    begin
        State.Set(Phil.Id, Thinking);
        Put_Line (Philosopher.Image(Phil));
        delay Delay_Sec;
    end Philosopher_Thinking;
    
    procedure Philosopher_Take_Fork (Phil: out Philosopher.Philosopher) is
    begin
       Waiter.Seize; 
       State.Set(Phil.Id, Hungry);
       Test(Phil);
       Waiter.Release;
       Forks(Phil.Id).Seize;
    end Philosopher_Take_Fork;

    procedure Philosopher_Put_Fork (Phil: Philosopher.Philosopher) is
    begin
        Waiter.Seize;
        State.Set(Phil.Id, Thinking);
        Test(Ph(Left(Phil.Id)));
        Test(Ph(Right(Phil.Id)));
        Waiter.Release;
    end Philosopher_Put_Fork;

    procedure Philiospher_Diner (Phil: out Philosopher.Philosopher; Eating_Sec: Duration := 1.0) is
    begin
        Philosopher_Thinking (Phil);
        Philosopher_Take_Fork (Phil); 
        Put_Line (Philosopher.Image(Phil));
        delay Eating_Sec;
        Philosopher_Put_Fork (Phil);
    end Philiospher_Diner;
    
    task type Philosopher_Handler is 
        entry Start(Phil: Philosopher.Philosopher);
    end Philosopher_Handler;

    task body Philosopher_Handler is
        Phil: Philosopher.Philosopher;
    begin
        accept Start (Phil: Philosopher.Philosopher) do
            Philosopher_Handler.Phil := Phil;
        end Start;
        
        -- for I in 1 .. 10 loop    
            Philiospher_Diner (Philosopher_Handler.Phil);
        -- end loop;
    end Philosopher_Handler;

    Philosopher_Parallel: array (1 .. Philosopher.GetPhilosopherNum) of Philosopher_Handler;
begin
    for I in Philosopher_Parallel'Range loop
        -- Ph(I).State := Hungry;
        Philosopher_Parallel(I).Start (Ph(I));
    end loop;

exception
    when e : Constraint_Error =>
        Put_Line
           ("[!] Divide by zero exception. Set another number of Philosophers (> 0).");

end Lab4;
