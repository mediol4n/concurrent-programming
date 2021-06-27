with Ada.Text_IO; use Ada.Text_IO;
with Ada.Numerics.Discrete_Random;
with Ada.Numerics.Float_Random;


package body graph is 
   
   procedure initGraph(n : Integer; d : Integer; b : Integer) is
      procedure initEdges is 
         subtype nodeRange is Integer range 0 .. n-1;
         package MyRandom is new Ada.Numerics.Discrete_Random(nodeRange);
         MyGenerator : MyRandom.Generator;
         Src : nodeRange;
         Dest : nodeRange;
         Repeated : Boolean;
         Counter : Natural;
      begin
         --initialize edges
         allEdges := new EveryEdge(0..n-1);
         for i in allEdges'Range loop
            allEdges(i) := new NodeNexts.Vector;
         end loop;
         
         --add default edges from node i to node i+1
         for i in 0..n-2 loop
            allEdges(i).all.Append(i+1);
         end loop;
         
         --adding extra d shortcuts
         Counter := 0;
         while Counter < d loop
            Repeated := false;
            Src := MyRandom.Random(MyGenerator);
            Dest := MyRandom.Random(MyGenerator);
            if Src+1 < Dest then 
               for j in 0..allEdges(Src).Last_Index loop
                  if allEdges(Src).Element(j) = Dest then
                     Repeated := true;
                  end if;
               end loop;
               if Repeated = false then
                  allEdges(Src).all.Append(Dest);
                  Counter := Counter + 1;
               end if;
            end if;
         end loop;
         Counter := 0;
         while Counter <= b loop
            Repeated := false;
            Src := MyRandom.Random(MyGenerator);
            Dest := MyRandom.Random(MyGenerator);
            if Dest < Src then
               for j in 0..allEdges(Src).Last_Index loop
                  if allEdges(Src).Element(j) = Dest then
                     Repeated := true;
                  end if;
               end loop;
               if Repeated = false then
                  allEdges(Src).all.Append(Dest);
                  Counter := Counter + 1;
               end if;
            end if;
         end loop;
         
         
         
         -- printing all nodes and theirs following nodes
         for i in 0..n-1 loop
            Put("Wierzcholek: "&Integer'Image(i)&", nastepniki: ");
            for j in 0 .. allEdges(i).Last_Index loop
               Put(Integer'Image(allEdges(i).Element(j))&" ");
               null;
            end loop;
            Put_Line(" ");
         end loop;
      end;
      
   begin 
      --initialize task array
      graph.n := n;
      initEdges;
      MyTaskArray := new TaskArray(0..n-1);
   end;
   
   task body NodeTask is 
      subtype IndexRange is Natural range 0 .. NodeTask.nexts.Last_Index;
      package RandomIndex is new Ada.Numerics.Discrete_Random (IndexRange);
      RandomIndexGenerator : RandomIndex.Generator;
      NextTask : Natural;
      DelayGenerator : Ada.Numerics.Float_Random.Generator;
      LocalPacket : Packet;
      MyTrap : Boolean := False;
   begin
      RandomIndex.Reset(RandomIndexGenerator);
      Ada.Numerics.Float_Random.Reset(DelayGenerator);
      loop
         
         select
            --we need to handle trap and package so we use select
            accept trap (hunterTrap : in Boolean) do
               MyTrap := hunterTrap;
            end trap; 
            delay Standard.Duration(Ada.Numerics.Float_Random.Random(DelayGenerator));
         or
            accept handle (myPacket : in Packet) do
                  --in this method we only get the packet and assign it to the local variable
                  LocalPacket := myPacket;
            end handle;   
            delay Standard.Duration(Ada.Numerics.Float_Random.Random(DelayGenerator));
            --send msg to printer 
            myPrinter.nodePrint(id, LocalPacket.id);
            --we increase packet's number of visited nodes
            LocalPacket.ttl := LocalPacket.ttl + 1;
            --if we have trap from hunter we send message to receiver that package is in trap
            if MyTrap = True then 
               myReceiver.packetTrapped(myPacket => LocalPacket);
               --and of course trap disappears
               MyTrap := false;
            else   
               --now we check if package is 'dead'. If yes we send special info to receiver
               if LocalPacket.ttl = h then
                  myReceiver.packetDied(myPacket => LocalPacket);
               else
                  -- if we are in last "node" we want printer to get the packet
                  if NodeTask.id = graph.n-1 then
                     myReceiver.receive(myPacket => LocalPacket);
                  else 
                     --otherwise we send packet forward
                     --delay Standard.Duration(Ada.Numerics.Float_Random.Random(DelayGenerator));
                     NextTask := NodeTask.nexts.Element(RandomIndex.Random(RandomIndexGenerator));
                     MyTaskArray.all(NextTask).handle(myPacket => LocalPacket);
                  end if;
               end if;
            end if;       
         or    
            --when there are no more packets we terminate    
            terminate;    
         end select;
      end loop;
   end;
   
   
   
   
   
   task body packetSender is
      PacketToSend : Packet;
      DelayGenerator : Ada.Numerics.Float_Random.Generator;
   begin
      Ada.Numerics.Float_Random.Reset(DelayGenerator);
      --sending method
      accept send(k : Natural) do
         for i in 0..k-1 loop
            --after a random amount of time, sender wakes up and sends another packet
            PacketToSend := Packet'(id => i, ttl => 0);
            delay Standard.Duration(Ada.Numerics.Float_Random.Random(DelayGenerator));
            MyTaskArray.all(0).handle(myPacket => PacketToSend);
         end loop;
      end send;
   end;
   
   
   task body packetReceiver is 
      -- we need it to know if we receive all packets (or lose)
      LocalK : Natural;
      DelayGenerator : Ada.Numerics.Float_Random.Generator;
   begin
      accept start (k : in Natural) do
         -- when start we initialize variables
         Ada.Numerics.Float_Random.Reset(DelayGenerator);
         LocalK := k;
      end start;
      loop 

         select
            accept receive(myPacket : in Packet) do
               --when we receive packet we increase packetsCounter
               PacketCounter := PacketCounter + 1;
               --and then we send message to printer
               myPrinter.packetReceived(myPacket.id);
               if PacketCounter = LocalK then   
               myPrinter.finalReport;
               end if;
               delay Standard.Duration(Ada.Numerics.Float_Random.Random(DelayGenerator)); 
            end receive;
         or
            accept packetDied (myPacket : in Packet) do
               --when packet died receiver must find out that to know when to call final report
               PacketCounter := PacketCounter + 1;
               --we send message to printer
               myPrinter.packetDied(myPacket.id);
               if PacketCounter = LocalK then   
                  myPrinter.finalReport;
               end if;
               delay Standard.Duration(Ada.Numerics.Float_Random.Random(DelayGenerator)); 
            end packetDied;
         or
            accept packetTrapped (myPacket : in Packet) do
               --the same as above
               PacketCounter := PacketCounter + 1;
               --we send message to printer
               myPrinter.packetTrapped(myPacket.id);
               if PacketCounter = LocalK then   
                  myPrinter.finalReport;
               end if;
               delay Standard.Duration(Ada.Numerics.Float_Random.Random(DelayGenerator)); 
            end packetTrapped;
         or terminate;
         end select;

      end loop;
   end;
   
   task body hunter is
      --ok, i know that we could make range but i don't know
      -- why it didn't work.  My solution with modulo works perfectly. 
      package MyRandom is new Ada.Numerics.Discrete_Random(Natural);
      DelayGenerator : Ada.Numerics.Float_Random.Generator;
      MyGenerator : MyRandom.Generator;
      index : Natural;
   begin
     
      accept start do
         --at start we only reset generators
         MyRandom.Reset(MyGenerator);
         Ada.Numerics.Float_Random.Reset(DelayGenerator);
      end start;
      for i in 0..2 loop --here we can put infinite loop but we dont want all packages to put in trap
         index := MyRandom.Random(MyGenerator) mod n;
         --we call function trap in random node
         MyTaskArray.all(index).trap(True);
         --we send message to printer
         myPrinter.hunterPutTrap(nodeId => index);
         delay Standard.Duration(Ada.Numerics.Float_Random.Random(DelayGenerator)); 
      end loop;
         
      
   end;
   

   
   task body printer is
      --variables
      LocalK : Natural;
      LocalN : Natural;
      NodesArray : NodesPacketsPtr;
      PacketsArray : PacketsNodesPtr;
   begin
      accept start (k : in Natural ; n : in Natural) do
         --when start we initialize variables
         LocalN := n-1;
         LocalK := k-1;
         PacketsArray := new PacketsNodes(0..k-1);
         NodesArray := new NodesPackets(0..n-1);
         
         for i in 0..k-1 loop
            PacketsArray(i) := new PrinterVector.Vector;
         end loop;
         
         for i in 0..n-1 loop
            NodesArray(i) := new PrinterVector.Vector;
         end loop;
         
      end start;
      loop
         select
            accept nodePrint (nodeId : in Natural ; packetId : in Natural) do
               --method prints information about where packet is
               Put_Line("Pakiet: "&Natural'Image(packetId)&" jest w wierzcholku: "&Natural'Image(nodeId));
               --we append info about packets and nodes for final reports
               NodesArray(nodeId).all.Append(packetId);
               PacketsArray(packetId).all.Append(nodeId);
            end nodePrint;
         or
            accept packetReceived (packetId : in Natural) do
               --method prints that receiver got packet
               Put_Line("Odbiorca otrzymal pakiet: "&Natural'Image(packetId));
            end packetReceived;
         or
            accept packetDied (packetId : in Natural) do
               --here we print info that package died (his ttl = h)
               Put_Line("Pakiet "&Natural'Image(packetId)&" umarl!");
            end packetDied;
         or
            accept packetTrapped (packetId : in Natural) do
               --here we print info that package fell into the trap
               Put_Line("Pakiet "&Natural'Image(packetId)&" wpadl w pulapke!");
            end packetTrapped;
         or
            accept hunterPutTrap (nodeId : in Integer) do
               --this method informs user where hunter put trap
               Put_Line("Klusownik stawia pulapke w "&Natural'Image(nodeId)&" wierzcholku");
            end hunterPutTrap;
         or
            accept finalReport  do
               --here we print final reports
               Put_Line("");
               Put_Line("###########################");
               Put_Line("#######FINAL REPORTS#######");
               Put_Line("###########################");
               Put_Line("");
               Put_Line("");
               --information about each node
               for i in 0..LocalN loop
                  Put("Wierzcholek: "&Natural'Image(i)&" zostal odwiedzony przez pakiety: ");
                  for j in 0..NodesArray(i).Last_Index loop
                     Put(Natural'Image(NodesArray(i).Element(j))&" ");
                  end loop;
                  Put_Line("");
               end loop;
               Put_Line("");
               
               --information about each packet
               for i in 0..LocalK loop
                  Put("Pakiet: "&Natural'Image(i)&" odwiedzil wierzcholki: ");
                  for j in 0..PacketsArray(i).Last_Index loop
                     Put(Natural'Image(PacketsArray(i).Element(j))&" ");
                  end loop;
                  Put_Line("");
               end loop;
               Put_Line("");
                        
            end finalReport;
            exit;
         end select;
      end loop;
   end printer;
   
            
end;
      
      
      
      
      
      
      
