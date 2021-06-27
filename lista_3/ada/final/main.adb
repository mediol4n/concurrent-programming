with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with graph; use graph;



procedure main is
   --our main parameters
   n : Natural;
   d : Natural;
begin
   --getting params from command line
   n := Integer'Value(Argument(1));
   d := Integer'Value(Argument(2));
   --dummy data
   --n := 3;
   --d := 1;
   initGraph(n, d);
   Put_Line(" ");
   Put_Line(" ");
   --starting all tasks
   for i in 0..n-1 loop
      graph.MyTaskArray(i) := new NodeTask(i, allEdges(i));
   end loop;
   Put_Line(" ");
   Put_Line(" ");
end main;


