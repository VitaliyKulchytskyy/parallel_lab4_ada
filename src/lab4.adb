with Philosopher;           use Philosopher;
with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Exceptions;        use Ada.Exceptions;
with GNAT.Semaphores;       use GNAT.Semaphores;
with Ada.Containers;        use Ada.Containers;

procedure Lab4 is
    Ph    :
       array
          (1 ..
                 Philosopher.GetPhilosopherNum) of aliased Philosopher
          .Philosopher;
    Forks :
       array
          (1 ..
                 Philosopher.GetPhilosopherNum) of Counting_Semaphore
          (1, Default_Ceiling);

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

    protected Host is
        entry Greet;
        procedure Farewell;
    private
        Guests : Integer := 0;
    end Host;

    protected body Host is
        entry Greet when Guests < Philosopher.GetPhilosopherNum is
        begin
            Guests := Guests + 1;
        end Greet;
        procedure Farewell is
        begin
            Guests := Guests - 1;
        end Farewell;
    end Host;

    task type Philosopher_Parallel (Id : Integer);
    task body Philosopher_Parallel is
    begin
        for Life_Cycle in 1 .. 10 loop
            Put_Line (Id'Image & " is thinking");
            delay 0.20;
            Put_Line (Id'Image & " is hungry");
            Host.Greet;
            Forks (Left (Id)).Seize;
            Forks (Right (Id)).Seize;
            Put_Line (Id'Image & " is eating");
            delay 0.20;
            Forks (Left (Id)).Release;
            Forks (Right (Id)).Release;
            Host.Farewell;
        end loop;
        Put_Line (Id'Image & " is leaving");
    end Philosopher_Parallel;

    Ph1 : Philosopher_Parallel (1);
    Ph2 : Philosopher_Parallel (2);
    Ph3 : Philosopher_Parallel (3);
    Ph4 : Philosopher_Parallel (4);
    Ph5 : Philosopher_Parallel (5);

begin
    -- for I in Ph'Range loop
    --      Put_Line ("ID: " & Ph(I).Id'Image);
    --      Put_Line ("Left: " & Left(Ph(I).Id)'Image);
    --      Put_Line ("Right: " & Right(Ph(I).Id)'Image);
    --      Put_Line("======");
    -- end loop;
    null;

exception
    when e : Constraint_Error =>
        Put_Line
           ("[!] Divide by zero exception. Set another number of Philosophers (> 0).");

end Lab4;
