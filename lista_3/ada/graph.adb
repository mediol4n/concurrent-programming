with Ada.Text_IO; use Ada.Text_IO;
with Ada.Numerics.Discrete_Random;
with Ada.Numerics.Float_Random;

package body graph is 
   
   procedure initGraph(n : Integer; d : Integer) is
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
         
         for i in n-1..1 loop
            allEdges(i).all.Append(i-1);
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
                  allEdges(Dest).all.Append(Src);
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

      protected type RT_Protection is
      procedure initialize;
      function checkNewCost(from_whom : Natural; newOffer : Offer) return Boolean;
      function checkAndSend(j : Natural; cost: out Natural) return Boolean;
      private
        rt : RoutingTablePtr;
      end RT_Protection;

      protected body RT_Protection is
         procedure initialize is
           element : RoutingTableElementPtr;
           subtype nodeRange is Integer range 0 .. n-1;
         begin
            rt := new RoutingTable.Vector;
            element := new RoutingTableElement;
            
            for j in nodeRange loop

               if nexts.all.Contains(j) then
                  element.nextHop := j;
                  element.cost := 1;
               elsif id < j then
                  element.nextHop := id + 1;
                  element.cost := j - id;
               elsif id > j then
                  element.nextHop := id - 1;
                  element.cost := id - j;
               else
                  element.nextHop := id;
                  element.cost := 0;
               end if;
               element.changed := true;
               rt.all.Append(element);
            end loop;
         end;


         function checkNewCost(from_whom : Natural; newOffer : Offer) return Boolean is
            myNewCost : Natural;
         begin
            myNewCost := newOffer.cost + 1;
            if myNewCost < rt.all(newOffer.vertex_index).cost then
               rt.all(newOffer.vertex_index).nextHop := from_whom;
               rt.all(newOffer.vertex_index).cost := myNewCost;
               rt.all(newOffer.vertex_index).changed := true;
               return true;
            else 
               return false;
            end if;
         end checkNewCost;

         function checkAndSend(j : Natural; cost: out Natural) return Boolean is
         begin
            if rt.all(j).changed = true then
               rt.all(j).changed := false;
               cost := rt.all(j).cost;
               return true;
            else
               return false;
            end if;
         end checkAndSend;
      end RT_Protection;


      task type receiver is
         entry start;
         entry handlePacket(from : Natural; pakiecik : Packet);
      end receiver;

      task body receiver is
         myFrom : Natural;
         myPacket : Packet;
      begin
         accept start do
            loop
               accept handlePacket(from : Natural; pakiecik : Packet) do
                  myFrom := from;
                  myPacket := pakiecik;
               end handlePacket;
               for i in 0..myPacket.offers.Last_Index loop
                  if RT_Protection.checkNewCost(myFrom, myPacket.offers(i)) = true then
                     Put_Line("Ustawiono nowe zasady w "&Natural'Image(myPacket.offers(i).vertex_index));
                  end if;
               end loop;
            end loop;
         end start;
      end receiver;

      task type sender is
         entry start;
      end sender;

      task body sender is
         DelayGenerator : Ada.Numerics.Float_Random.Generator;
         cost : Natural;
         newPacket : Packet;
         newOffers : OffersVector;
         neighbour : Natural;
         resp : Boolean;
      begin
         accept start do
            loop
               cost := 1;
               delay Standard.Duration(Ada.Numerics.Float_Random.Random(DelayGenerator));
               for j in 0..n-1 loop
                  resp := RT_Protection.checkAndSend(j, cost);
                  if resp = true then
                     newOffers.all.append(Offer'(j, cost));
                  end if;
               end loop;

               if not newOffers.isEmpty then
                  newPacket := Packet'(id, newOffers);
                  for j in 0..nexts.Last_Index loop
                     neighbour := NodeTask.nexts.Element(j);
                     MyTaskArray.all(neighbour).handlePacket(id, newOffers);
                  end loop;
               end if;

            end loop;
         end start;
      end sender;

      myReceiver : receiver;
      mySender : sender;

      begin
         RT_Protection.initialize;
         mySender.start;
         myReceiver.start;
         loop
            accept handlePacket(from : Natural; pakiecik : Packet) do
               myReceiver.handlePacket(from, pakiecik);
            end handlePacket;
         end loop;


   end;
   


   
            
end;
