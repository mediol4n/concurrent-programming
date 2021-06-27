package main

import (
	"fmt"
	"math"
	"math/rand"
	"time"
)

type Vertex struct {
	id         int
	neighbours []Vertex
	R          []R_Element
	rescChan   chan Packet
}

type R_Element struct {
	nexthop int
	cost    int
	changed bool
}

type Locker struct {
	lock_R chan bool
	unlock_R chan bool
}

type Offer struct {
	j int
	cost int
}

type Packet struct {
	from   int
	offers []Offer
}

//var s = rand.NewSource(time.Now().UnixNano()) // seed of a pseudorandom number generator
//var r = rand.New(s)
var graph []Vertex // our main graph
var finishChan chan bool //channel to count how many vertexes finished work

func main() {
	var n = 10
	finishChan = make(chan bool)
	graph = initGraph(n, 20)
	initRoutingTable(n)
	printGraph()

	for i := 0; i < n; i++ {
		go vertex(i)
	}

	var counter int = 0
	for {
		<-finishChan
		counter++
		if counter == n {
			fmt.Println("Koniec pracy🏁")
			return
		}
	}

}

// Function initialize graph
func initGraph(n, d int) []Vertex {
	newGraph := make([]Vertex, n)
	newGraph[n-1] = Vertex{id: n - 1, rescChan: make(chan Packet, 1)}
	// initialize every vertex and add default neighbours
	for i := n - 2; i >= 0; i-- {
		newGraph[i] = Vertex{id: i, rescChan: make(chan Packet, 1)}
		newGraph[i].neighbours = append(newGraph[i].neighbours, newGraph[i+1])
	}
	for i := 1; i < n; i++ {
		newGraph[i].neighbours = append(newGraph[i].neighbours, newGraph[i-1])
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
		fmt.Println(id, "id", numOfNeighbours, " neighbours")
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
				packet := Packet{id, offers}
				fmt.Println("🟢Wierzchołek ", id, "wysyła oferty", packet.offers, "do sasiadow")
				//We send packages after unlock routing table to avoid "drowning out" every change in Routing table
				for i := 0; i < numOfNeighbours; i++ {
					graph[id].neighbours[i].rescChan <- packet
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
			packet := <-graph[id].rescChan
			fmt.Println("🔵Wierzcholek ", id, " otrzymal ", packet.offers, " od ", packet.from)
			lock := Locker{make(chan bool), make(chan bool)}
			receiverLocker <- lock
			<-lock.lock_R
			for j := 0; j < len(packet.offers); j++ {
				newCost := packet.offers[j].cost + 1
				if newCost < graph[id].R[j].cost {
					graph[id].R[j].cost = newCost
					graph[id].R[j].nexthop = packet.from
					graph[id].R[j].changed = true
					fmt.Println("🔵Routing Table w ", id, " ustawia na ", j, ": changed = false", "nextHop = ", packet.from, " changed = true")
				}
			}
			lock.unlock_R <- true
		}
	}()
}

