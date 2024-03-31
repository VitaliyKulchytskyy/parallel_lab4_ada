with Philosopher;           use Philosopher;
with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Exceptions;        use Ada.Exceptions;
with GNAT.Semaphores;       use GNAT.Semaphores;

procedure Lab4 is
    type Philosopher_Ptr is access all Philosopher.Philosopher;
    Ph : array (1 .. Philosopher.GetPhilosopherNum) of Philosopher.Philosopher;
    Forks :
       array
          (1 .. Philosopher.GetPhilosopherNum) of Counting_Semaphore (1, Default_Ceiling);
    Phil  : Integer;
    Waiter : Counting_Semaphore(1, Default_Ceiling);

    procedure Philosopher_Start_Eating
       (Phil       : out Philosopher.Philosopher; 
        Delay_Sec  : Duration := 0.5;
        Is_Verbose : Boolean  := True)
    is
    begin
        -- if Phil.State /= Hungry then
        --     return;
        -- end if;

        Waiter.Seize;

        Forks (Phil.Left_Fork_Id).Seize;
        if Is_Verbose then
            Put_Line
               ("The fork" & Phil.Left_Fork_Id'Img & " was tooken (by " &
                Philosopher.Image (Phil) & ").");
        end if;

        Forks (Phil.Right_Fork_Id).Seize;
        if Is_Verbose then
            Put_Line
               ("The fork" & Phil.Right_Fork_Id'Img & " was tooken (by " &
                Philosopher.Image (Phil) & ").");
        end if;

        -- Phil.State := Eating;
        delay Delay_Sec;

        Forks (Phil.Left_Fork_Id).Release;
        if Is_Verbose then
            Put_Line
               ("The fork" & Phil.Left_Fork_Id'Img & " was putten (by " &
                Philosopher.Image (Phil) & ").");
        end if;

        Forks (Phil.Right_Fork_Id).Release;
        if Is_Verbose then
            Put_Line
               ("The fork" & Phil.Right_Fork_Id'Img & " was putten (by " &
                Philosopher.Image (Phil) & ").");
        end if;

        Waiter.Release;

        -- Phil.State := Thinking;
    end Philosopher_Start_Eating;

    task type Philosopher_Handler is 
        entry Start(Phil: Philosopher.Philosopher);
    end Philosopher_Handler;

    task body Philosopher_Handler is
        Phil: Philosopher.Philosopher;
    begin
        accept Start (Phil: Philosopher.Philosopher) do
            Philosopher_Handler.Phil := Phil;
        end Start;
        
        for I in 1 .. 10 loop    
            Philosopher_Start_Eating (Philosopher_Handler.Phil);
        end loop;
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
