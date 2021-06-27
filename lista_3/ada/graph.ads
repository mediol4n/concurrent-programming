with Ada.Containers.Vectors;
package graph is
   
   --structure created for easier initializing graph
   package NodeNexts is new Ada.Containers.Vectors
     (Index_Type => Natural,
      Element_Type => Natural);
   
   type NodeNextsPtr is access NodeNexts.Vector;
   type EveryEdge is array (Natural range<>) of NodeNextsPtr;
   type EveryEdgePtr is access EveryEdge;

   type Offer is record
      vertex_index : Integer;
      cost : Integer;
   end record;

   type OfferPtr is access Offer;

   package OffersVector is new Ada.Containers.Vectors
      (Index_Type => Natural,
      Element_Type => OfferPtr);
   
   
   -- Package structure 
   type Packet is record
      source : Natural;
      offers : OffersVector;
   end record;
   
   type RoutingTableElement is record
      nextHop : Integer;
      cost : Integer;
      changed : Boolean;
   end record;
   
   type RoutingTableElementPtr is access RoutingTableElement;
      
   package RoutingTable is new Ada.Containers.Vectors
     (Index_Type => Natural, Element_Type => RoutingTableElementPtr);
   
   type RoutingTablePtr is access RoutingTable.Vector;
      
   
   -- node worker task it receives packeges and send it to one of the following nodes
   task type NodeTask (id : Natural; nexts : NodeNextsPtr) is
      entry handlePacket(from : Natural; pakiecik : Packet);
   end NodeTask;
   type NodeTaskPtr is access NodeTask;
   
   --array created to keep all nodetasks in array for easier management
   type TaskArray is array (Natural range<>) of NodeTaskPtr;
   type TaskArrayPtr is access TaskArray;
   
  
   -- it initaializes graph and print it in terminal
   procedure initGraph(n: Integer; d: Integer);
   
   

   
   
   -- Variables used in main.adb
   n : Natural;
   allEdges : EveryEdgePtr;
   MyTaskArray : TaskArrayPtr; 
   
   
end graph;
