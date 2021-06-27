with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with graph; use graph;



procedure main is
   --our main parameters
   k : Natural;
   d : Natural;
   h : Natural;
   b : Natural;
begin
   --getting params from command line
   n := Integer'Value(Argument(1));
   d := Integer'Value(Argument(2));
   k := Integer'Value(Argument(3));
   h := Integer'Value(Argument(4));
   b := Integer'Value(Argument(5));
   --n := 15;
   --k := 6;
   --d := 0;
   --h := 10;
   --b := 1;
   initGraph(n, d, b);
   Put_Line(" ");
   Put_Line(" ");
   --starting all tasks
   myReceiver.start(k);
   myPrinter.start(k, n);
   for i in 0..n-1 loop
      graph.MyTaskArray(i) := new NodeTask(i, allEdges(i), h);
   end loop;
   Put_Line(" ");
   Put_Line(" ");
   myHunter.start;
   mySender.send(k);
end main;

