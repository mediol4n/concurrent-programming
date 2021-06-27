package main

import (
	"fmt"
	"math"
	"math/rand"
	"time"
)

type Vertex struct {
	id            int
	neighbours    []Vertex
	R             []R_Element
	hosts         []Host
	rescChan      chan Packet_Offer
	forwarderChan chan Packet_Standard
	queue         chan Packet_Standard
}

type R_Element struct {
	nexthop int
	cost    int
	changed bool
}

type Locker struct {
	lock_R   chan bool
	unlock_R chan bool
}

type Offer struct {
	j    int
	cost int
}

type Packet_Offer struct {
	from   int
	offers []Offer
}

//New structers for ex 4

type Host_Pair struct {
	router_index int
	host_index   int
}

type Packet_Standard struct {
	sender_host     Host_Pair
	receiver_host   Host_Pair
	visited_routers []int
}

type Host struct {
	router_index int
	host_index   int
	rescChan     chan Packet_Standard
}

var graph []Vertex       // our main graph
var finishChan chan bool //channel to count how many vertexes finished work
var hosts []Host
var numOfHosts = 0

func main() {
	var n = 10
	finishChan = make(chan bool)
	graph = initGraph(n, 25)
	initRoutingTable(n)
	fmt.Println("﹌﹌﹌﹌﹌﹌﹌﹌﹌﹌﹌﹌﹌﹌﹌﹌﹌﹌﹌﹌﹌﹌﹌﹌﹌﹌﹌﹌﹌﹌﹌﹌﹌")
	printGraph()
	fmt.Println("﹌﹌﹌﹌﹌﹌﹌﹌﹌﹌﹌﹌﹌﹌﹌﹌﹌﹌﹌﹌﹌﹌﹌﹌﹌﹌﹌﹌﹌﹌﹌﹌﹌")
	printHosts()
	fmt.Println("﹌﹌﹌﹌﹌﹌﹌﹌﹌﹌﹌﹌﹌﹌﹌﹌﹌﹌﹌﹌﹌﹌﹌﹌﹌﹌﹌﹌﹌﹌﹌﹌﹌")
	fmt.Println("")
	fmt.Println("")

	for i := 0; i < numOfHosts; i++ {
		go host(hosts[i].host_index, hosts[i].router_index)
	}

	for i := 0; i < n; i++ {
		go vertex(i)
	}

	var counter int = 0
	for {
		<-finishChan
		counter++
		if counter == n+2 {
			fmt.Println("Protokół routingu zakończony! 🏁")
			//return
			break
		}
	}

}

// Function initialize graph
func initGraph(n, d int) []Vertex {
	newGraph := make([]Vertex, n)
	newGraph[n-1] = Vertex{id: n - 1, rescChan: make(chan Packet_Offer, 1)}
	// initialize every vertex and add default neighbours
	for i := n - 2; i >= 0; i-- {
		newGraph[i] = Vertex{id: i, rescChan: make(chan Packet_Offer, 1)}
		newGraph[i].neighbours = append(newGraph[i].neighbours, newGraph[i+1])
	}
	for i := 1; i < n; i++ {
		newGraph[i].neighbours = append(newGraph[i].neighbours, newGraph[i-1])
	}
	for i := 0; i < n; i++ {
		newGraph[i].hosts = generateHosts(i)
		newGraph[i].forwarderChan = make(chan Packet_Standard, 1)
		newGraph[i].queue = make(chan Packet_Standard)
	}
	var src, dest int = 0, 0
	// add d extra edges (short-cuts)
	for i := 1; i <= d; i++ {
		src = rand.Intn(n)
		dest = rand.Intn(n)
		// We need to check if the given path already exists
		if (src == dest) || (src == dest+1) || (dest == src+1) {
			i--
			continue
		}
		// We add new edge
		if !find(newGraph[src].neighbours, newGraph[dest]) {
			newGraph[src].neighbours = append(newGraph[src].neighbours, newGraph[dest])
			newGraph[dest].neighbours = append(newGraph[dest].neighbours, newGraph[src])
		} else {
			i--
		}

	}

	return newGraph
}

//Finds if given connection exists to avoid edge doubling
func find(nexts []Vertex, value Vertex) bool {
	for i := 0; i < len(nexts); i++ {
		if nexts[i].id == value.id {
			return true
		}
	}
	return false
}

func initRoutingTable(n int) {
	for i := 0; i < n; i++ {
		for j := 0; j < n; j++ {
			var R_Element R_Element = R_Element{}
			if find(graph[i].neighbours, graph[j]) {
				R_Element.nexthop = j
				R_Element.cost = 1
			} else if i < j {
				R_Element.nexthop = i + 1
				R_Element.cost = int(math.Abs(float64(i - j)))
			} else if i > j {
				R_Element.nexthop = i - 1
				R_Element.cost = int(math.Abs(float64(i - j)))
			}
			R_Element.changed = true
			graph[i].R = append(graph[i].R, R_Element)
		}
	}

}

// Function created for visualizing graph in command line
// It shows every vertex and its possible closest neighbours
// By default, as in a line graph, vertex with index i has an arrow to vertex with index  i + 1
func printGraph() {
	fmt.Println(" ")
	fmt.Println(" ")
	for i := 0; i < len(graph); i++ {
		fmt.Print("Wierzchołek: ", i, "\n sasiedzi ⟾  ")
		for j := 0; j < len(graph[i].neighbours); j++ {
			if graph[i].neighbours[j].id > graph[i].id {
				fmt.Print(graph[i].neighbours[j].id, " ")
			}
		}
		fmt.Println(" ")
		fmt.Println(" ")
	}
}

func vertex(id int) {
	senderLocker := make(chan Locker)
	receiverLocker := make(chan Locker)
	forwarderLocker := make(chan Locker)

	//forwarder
	go func() {
		//s := rand.NewSource(time.Now().UnixNano() + int64(id))
		//r := rand.New(s)

		//forwarder receiver
		go func() {
			for {
				select {
				case packet := <-graph[id].forwarderChan:
					//fmt.Println("Forwarder ", id, "received ", packet.sender_host.host_index)
					packet.visited_routers = append(packet.visited_routers, id)
					graph[id].queue <- packet
					//time.Sleep(2* time.Second)
				default:
					continue
				}
			}
		}()

		//forwarder sender
		go func() {
			for {
				packet := <-graph[id].queue
				target_router := packet.receiver_host.router_index
				//send packet to nexthop on road to another router on the route to receiever router
				lock := Locker{make(chan bool), make(chan bool)}
				forwarderLocker <- lock
				<-lock.lock_R
				if target_router == id {
					target_host := packet.receiver_host.host_index
					i := findHost(graph[id].hosts, target_host)
					if i < len(graph[id].hosts) {
						graph[id].hosts[i].rescChan <- packet
					}
				} else {
					graph[graph[id].R[packet.receiver_host.router_index].nexthop].forwarderChan <- packet
				}

				lock.unlock_R <- true
				//time.Sleep(time.Duration(r.Float64() * float64(time.Second)))
			}
		}()

	}()

	//time.Sleep(time.Second * 10)
	//stateful goroutine - server
	go func() {
		for {
			select {
			case sender := <-senderLocker:
				sender.lock_R <- true
				<-sender.unlock_R
			case receiver := <-receiverLocker:
				receiver.lock_R <- true
				<-receiver.unlock_R
			case forwarder := <-forwarderLocker:
				forwarder.lock_R <- true
				<-forwarder.unlock_R
			default:
				continue
			}
		}
	}()

	//sender
	go func() {
		var isFinished int = 0
		s := rand.NewSource(time.Now().UnixNano() + int64(id))
		r := rand.New(s)
		tableSize := len(graph[id].R)
		numOfNeighbours := len(graph[id].neighbours)
		for {
			time.Sleep(time.Duration(r.Float64() * float64(time.Second)))
			lock := Locker{make(chan bool), make(chan bool)}
			senderLocker <- lock
			<-lock.lock_R
			var offers []Offer
			for j := 0; j < tableSize; j++ {
				if graph[id].R[j].changed == true && j != id {
					offers = append(offers, Offer{j, graph[id].R[j].cost})
					graph[id].R[j].changed = false
					fmt.Println("🟡Routing table w ", id, " ustawia na ", j, ": changed = false")
				}
			}

			lock.unlock_R <- true

			if len(offers) > 0 {
				packet_Offer := Packet_Offer{id, offers}
				//fmt.Println("🟢Wierzchołek ", id, "wysyła oferty", packet_Offer.offers, "do sasiadow")
				//We send packages after unlock routing table to avoid "drowning out" every change in Routing table
				for i := 0; i < numOfNeighbours; i++ {
					graph[id].neighbours[i].rescChan <- packet_Offer
				}
			} else {
				isFinished++
				if isFinished == numOfNeighbours {
					finishChan <- true
				}
			}
		}
	}()

	//receiver
	go func() {
		s := rand.NewSource(time.Now().UnixNano() + int64(id))
		r := rand.New(s)
		for {
			time.Sleep(time.Duration(r.Float64() * float64(time.Second)))
			packet_Offer := <-graph[id].rescChan
			//fmt.Println("🔵Wierzcholek ", id, " otrzymal ", packet_Offer.offers, " od ", packet_Offer.from)
			lock := Locker{make(chan bool), make(chan bool)}
			receiverLocker <- lock
			<-lock.lock_R
			for j := 0; j < len(packet_Offer.offers); j++ {
				newCost := packet_Offer.offers[j].cost + 1
				if newCost < graph[id].R[j].cost {
					graph[id].R[j].cost = newCost
					graph[id].R[j].nexthop = packet_Offer.from
					graph[id].R[j].changed = true
					fmt.Println("🔵Routing Table w ", id, " ustawia na ", j, ": changed = false", "nextHop = ", packet_Offer.from, " changed = true")
				}
			}
			lock.unlock_R <- true
		}
	}()

}

func host(index int, router_id int) {
	//we are sure that this id exists so we dont have to check it
	id := findHost(graph[router_id].hosts, index)
	createRandomStandardPacket(index, router_id)
	s := rand.NewSource(time.Now().UnixNano() + int64(index))
	r := rand.New(s)

	for {
		select {
		case packet := <-graph[router_id].hosts[id].rescChan:
			toPrint := fmt.Sprint("📥 Host(", index, ", ", router_id, ") receieved packet from Host(", packet.sender_host.host_index, ", ", packet.sender_host.router_index, ") 📥\n")
			toPrint = toPrint + fmt.Sprint("Visited Routers ⟾ ")
			for i := 0; i < len(packet.visited_routers); i++ {
				toPrint = toPrint + fmt.Sprint(packet.visited_routers[i], ", ")
			}
			fmt.Println(toPrint)

			time.Sleep(time.Duration(r.Float64() * float64(time.Second)))
			if len(packet.visited_routers) > 0 {
				createReversePacket(index, router_id, packet.sender_host.host_index, packet.sender_host.router_index)
			} else {
				createRandomStandardPacket(index, router_id)
			}
		default:
			time.Sleep(time.Duration(r.Float64() * float64(time.Second)))
			continue

		}

	}

}

/*------------------ Functions for creating Standard Packages------------------*/
func createRandomStandardPacket(index int, router_id int) {
	next_router := rand.Intn(len(graph))
	var length = len(graph[next_router].hosts)
	router_host := rand.Intn(length)
	next_host := graph[next_router].hosts[router_host].host_index
	sender := Host_Pair{router_index: router_id, host_index: index}
	reciever := Host_Pair{router_index: next_router, host_index: next_host}
	var visited_routers []int
	//fmt.Println("RANDOM ->", Packet_Standard{sender_host: sender,
	//	receiver_host: reciever, visited_routers: visited_routers})
	graph[router_id].forwarderChan <- Packet_Standard{sender_host: sender,
		receiver_host: reciever, visited_routers: visited_routers}
}

func createReversePacket(sender_id int, sender_router_id int, receiver_id int, receiver_router_id int) {
	sender := Host_Pair{host_index: sender_id, router_index: sender_router_id}
	receiver := Host_Pair{host_index: receiver_id, router_index: receiver_router_id}
	var visited_routers []int
	//fmt.Println("REVERSE ->", Packet_Standard{sender_host: sender,
	//	receiver_host: receiver, visited_routers: visited_routers})
	graph[sender_router_id].forwarderChan <- Packet_Standard{sender_host: sender,
		receiver_host: receiver, visited_routers: visited_routers}
}

func findHost(hosts []Host, id int) int {
	for i, n := range hosts {
		if id == n.host_index {
			return i
		}
	}

	return len(hosts)
}

func generateHosts(router int) []Host {
	howMany := rand.Intn(3) + 1
	newHosts := make([]Host, howMany)

	for i := 0; i < howMany; i++ {
		newHosts[i] = Host{router_index: router, host_index: numOfHosts, rescChan: make(chan Packet_Standard)}
		numOfHosts++
		hosts = append(hosts, newHosts[i])
	}
	return newHosts
}

func printHosts() {
	for i := 0; i < len(graph); i++ {
		fmt.Println("Wierzchołek ", i)
		fmt.Print("hosty => ")
		for j := 0; j < len(graph[i].hosts); j++ {
			fmt.Print(graph[i].hosts[j].host_index, ", ")
		}
		fmt.Println(" ")
		fmt.Println(" ")
	}
}
