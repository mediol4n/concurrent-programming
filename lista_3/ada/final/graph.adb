with Ada.Text_IO; use Ada.Text_IO;
with Ada.Numerics.Discrete_Random;
with Ada.Numerics.Float_Random;
with Ada.Strings.Unbounded;


package body graph is 

   package Str renames Ada.Strings.Unbounded;
   
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
         allEdges(0).all.Append(1);
         for i in 1..n-2 loop
            allEdges(i).all.Append(i+1);
            allEdges(i).all.Append(i-1);
         end loop;
         allEdges(n-1).all.append(n-2);
         
         
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
            
            for j in 0..n-1 loop
               element := new RoutingTableElement;
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
            if rt.all(j).changed = true and j /= id then
               rt.all(j).changed := false;
               cost := rt.all(j).cost;
               return true;
            else
               return false;
            end if;
         end checkAndSend;


      end RT_Protection;

      myRTProtection : RT_Protection;
      


      task type receiver is
         entry start;
         entry handlePacket(from : Natural; pakiecik : Packet);
         entry finish;
      end receiver;

      task body receiver is
         myFrom : Natural;
         myPacket : Packet;
      begin
         select
         accept start;
            loop
               select
               accept handlePacket(from : Natural; pakiecik : Packet) do
                  myFrom := from;
                  myPacket := pakiecik;
               end handlePacket;
               Put_Line("🔵Wierzcholek "&Natural'Image(id)&" otrzymal pakiet od "&Natural'Image(myPacket.source));
               for i in 0..myPacket.offers.Last_Index loop
                  if myRTProtection.checkNewCost(from_whom => myFrom, newOffer => myPacket.offers.Element(i)) = true then
                     Put_Line("Ustawiono nowe wartości routing table i zmieniono changed = true w "&Natural'Image(myPacket.offers(i).vertex_index));
                  end if;
               end loop;
               or
                  accept finish;
                  delay Standard.Duration(0.5);
                  exit;
               end select;
            end loop;
         or
            terminate;
         end select;
      end;

      task type sender is
         entry start;
      end sender;

      task body sender is
         DelayGenerator : Ada.Numerics.Float_Random.Generator;
         cost : Natural;
         newPacket : Packet;
         newOffers : OffersVector.Vector;
         neighbour : Natural;
         retString : Str.Unbounded_String;
         isFinished : Integer;
         numOfNeighbours : Integer;

         function printPacket return Str.Unbounded_String is
            retString : Str.Unbounded_String := Str.Null_Unbounded_String;
         begin
            Str.Append(Source   => retString,
                       New_Item => "Wierzcholek " & Natural'Image(id) & " wysyła [");
            for i in 0..newOffers.Last_Index loop
               Str.Append(Source   => retString,
                       New_Item => "{"&Natural'Image(newOffers.Element(i).vertex_index)&", "&Natural'Image(newOffers.Element(i).cost)&"}, ");
            end loop;
            Str.Append(Source   => retString,
                       New_Item => "] do sasiadow.");
            return retString;
         end;

      begin
         isFinished := 0;
         numOfNeighbours := nexts.Last_Index+1;
         Ada.Numerics.Float_Random.reset(DelayGenerator);
         select
         accept start;
            loop
               cost := 1;
               delay Standard.Duration(Ada.Numerics.Float_Random.Random(DelayGenerator));
               newOffers.Clear;
               for j in 0..n-1 loop
                  if myRTProtection.checkAndSend(j, cost) = true then 
                     newOffers.append(Offer'(j, cost));
                     Put_Line("----->>Wierzcholek "&Natural'Image(id)&" ustawil w "&Natural'Image(j)&" changed = false");
                  end if;
               end loop;

               if not newOffers.is_Empty then
                  newPacket := Packet'(id, newOffers);
                  retString := printPacket;
                  Put_Line(Str.To_String(retString));
                  
                  for j in 0..nexts.Last_Index loop
                     neighbour := NodeTask.nexts.Element(j);
                     --Put_Line(Natural'Image(id)&"<J>:="&Natural'Image(neighbour));
                     MyTaskArray.all(neighbour).handlePacket(id, newPacket);

                  end loop;
               else 
                  isFinished := isFinished + 1;
                  if isFinished = numOfNeighbours then
                     myTerminator.finish;
                     exit;
                  end if;
               end if;
            end loop;
         or
           terminate;
         end select;
      end;

      myReceiver : receiver;
      mySender : sender;

      begin
         myRTProtection.initialize;
         mySender.start;
         myReceiver.start;
         loop
            select
            accept handlePacket(from : Natural; pakiecik : Packet) do
               myReceiver.handlePacket(from, pakiecik);
            end handlePacket;
            or
            accept finish;
            myReceiver.finish;
            exit;
            end select;
         end loop;


   end;
   

   task body Terminator is
      Counter : Integer;
   begin
      Counter := 0;
      loop
         select
            accept finish do
               Counter := Counter + 1;
            end finish;
            if Counter = n then
                  for i in 0..n-1 loop
                     MyTaskArray(i).finish;
                  end loop;
                  exit;
            end if;
         end select;
      end loop;
   end;
            
end;
