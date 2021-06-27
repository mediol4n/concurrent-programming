with Ada.Containers.Vectors;
package graph is
   
   PacketCounter : Integer := 0;
   
   --structure created for easier initializing graph
   package NodeNexts is new Ada.Containers.Vectors
     (Index_Type => Natural,
      Element_Type => Natural);
   
   type NodeNextsPtr is access NodeNexts.Vector;
   type EveryEdge is array (Natural range<>) of NodeNextsPtr;
   type EveryEdgePtr is access EveryEdge;
   
   
   -- Package structure 
   type Packet is record
      id : Natural;
      ttl : Natural;
   end record;
   
   -- node worker task it receives packeges and send it to one of the following nodes
   task type NodeTask (id : Natural; nexts : NodeNextsPtr; h : Natural) is 
      -- special method which only get the packet
      entry handle(myPacket : Packet); 
      entry trap(hunterTrap : Boolean);
   end NodeTask;
   type NodeTaskPtr is access NodeTask;
   
   --array created to keep all nodetasks in array for easier management
   type TaskArray is array (Natural range<>) of NodeTaskPtr;
   type TaskArrayPtr is access TaskArray;
   
  
   -- it initaializes graph and print it in terminal
   procedure initGraph(n: Integer; d: Integer; b: Integer);
   
   
   -- task that sends packets
   task type packetSender is 
      entry send(k : Natural);
   end packetSender;
   
   
   -- task that receives packets from last node
   task type packetReceiver is 
      entry start(k : Natural);
      entry receive(myPacket : Packet);
      entry packetDied(myPacket : Packet);
      entry packetTrapped(myPacket : Packet);
   end packetReceiver;
   
   -- task that put traps in nodes
   task type hunter is
      entry start;
   end hunter;
   
   
   -- printing server, it also prints final reports
   task type printer is
      entry start(k : Natural; n: Natural);
      entry nodePrint(nodeId : Natural; packetId : Natural);
      entry packetReceived(packetId : Natural);
      entry packetDied(packetId : Natural);
      entry packetTrapped(packetId : Natural);
      entry hunterPutTrap(nodeId : Integer);
      entry finalReport;
   end printer;
   
   
   package PrinterVector is new Ada.Containers.Vectors
     (Index_Type => Natural,
      Element_Type => Natural);
   type PrinterVectorPtr is access PrinterVector.Vector;
   
   --We will append here every node with packets which visited it
   type NodesPackets is array (Natural range<>) of PrinterVectorPtr;
   type NodesPacketsPtr is access NodesPackets;
   -- We will append here every node where each packet was
   type PacketsNodes is array (Natural range<>) of PrinterVectorPtr; 
   type PacketsNodesPtr is access PacketsNodes;
   
   
   
   
   -- Variables used in main.adb
   myPrinter : printer;
   n : Natural;
   allEdges : EveryEdgePtr;
   mySender : packetSender;
   MyTaskArray : TaskArrayPtr; 
   myReceiver : packetReceiver;
   myHunter : hunter;
   
   
end graph;
