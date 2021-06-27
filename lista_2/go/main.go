package main

import (
	"fmt"
	"math/rand"
	"time"
)

// globaly available variables
var graph []Vertex // our main graph
//var s = rand.NewSource(time.Now().UnixNano()) // seed of a pseudorandom number generator
//var r = rand.New(s)                           // generator
var done = make(chan bool)               // channel which informs us about simulation finish to synchronize routines
var finalReport = make(chan bool)        // channel which informs printer to print results
var packageCounter int = 0               // used to count received packages
var n, d, k, b, h int                    // variables scanned at program startup
var packages []Package                   // slice of packages used to print results
var receiverChan = make(chan Package, 1) // channel for receiver
var stdout = make(chan string)           // channel for printer
var deadPackages = 0                     // counter to know how many packages died
var hunterChan = make(chan bool)

// vertex structure with atributes
type Vertex struct {
	id         int
	nexts      []Vertex
	recChannel chan Package
	visited    []int
	trap       chan bool
}

// package structure with atributes
type Package struct {
	id      int
	visited []int
	ttl     int
}

func main() {
	fmt.Println("Enter n, d, k, b and h")
	fmt.Scanln(&n, &d, &k, &b, &h)
	graph = initGraph(n, d, b)
	printGraph()
	fmt.Println(" ")
	fmt.Println(" ")
	go packageSender(k)
	for i := 0; i < n; i++ {
		go vertexWorker(i)
		fmt.Println("Uruchamiam wierzcholek ", i)
	}
	fmt.Println(" ")
	fmt.Println(" ")
	go packageReceiver(n)
	go printer()
	go hunter()
	<-done
}

/* --------------------------------- CREATING GRAPH --------------------------------- */

// Function initialize graph
func initGraph(n, d, b int) []Vertex {
	newGraph := make([]Vertex, n)
	newGraph[n-1] = Vertex{id: n - 1, recChannel: make(chan Package, 1), trap: make(chan bool, 1)}
	// initialize every vertex and add default neighbours
	for i := n - 2; i >= 0; i-- {
		newGraph[i] = Vertex{id: i, recChannel: make(chan Package, 1), trap: make(chan bool, 1)}
		newGraph[i].nexts = append(newGraph[i].nexts, newGraph[i+1])
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

		if dest < src {
			tmp := dest
			dest = src
			src = tmp
		}
		// We add new edge
		if !find(newGraph[src].nexts, newGraph[dest]) {
			newGraph[src].nexts = append(newGraph[src].nexts, newGraph[dest])
		} else {
			i--
		}

	}

	for i := 1; i <= b; i++ {
		src = rand.Intn(n)
		dest = rand.Intn(n)

		if src == dest {
			i--
			continue
		}

		if src < dest {
			tmp := dest
			dest = src
			src = tmp
		}
		if !find(newGraph[src].nexts, newGraph[dest]) {
			newGraph[src].nexts = append(newGraph[src].nexts, newGraph[dest])
		} else {
			i--
		}
	}

	return newGraph
}

// Function created for visualizing graph in command line
// It shows every vertex and its possible closest neighbours
// By default, as in a line graph, vertex with index i has an arrow to vertex with index  i + 1
func printGraph() {
	fmt.Println(" ")
	fmt.Println(" ")
	for i := 0; i < len(graph); i++ {
		fmt.Print("Wierzchołek: ", i, ", następniki: ")
		for j := 0; j < len(graph[i].nexts); j++ {
			fmt.Print(graph[i].nexts[j].id, " ")
		}
		fmt.Println(" ")
	}
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

/*--------------------------------- PACKAGE SENDING ---------------------------------*/

//Package sender. Every given random time places the vertex at the zero vertex
func packageSender(k int) {
	var s = rand.NewSource(time.Now().UnixNano()) // seed of a pseudorandom number generator
	var r = rand.New(s)                           // generator
	for i := 0; i < k; i++ {
		graph[0].recChannel <- Package{id: i, ttl: 0}
		time.Sleep(time.Duration(2 * r.Float64() * float64(time.Second)))
	}
}

//Vertex function to receive the package and forward it
func vertexWorker(vertexId int) {
	var s = rand.NewSource(time.Now().UnixNano()) // seed of a pseudorandom number generator
	var r = rand.New(s)                           // generator
	var isTrapped = false
	for {
		time.Sleep(time.Duration(r.Float64() * float64(time.Second)))
		select {
		case msg := <-graph[vertexId].trap:
			isTrapped = msg
		case msg := <-graph[vertexId].recChannel:
			//we put message to printing channel
			msg.ttl++
			stdout <- fmt.Sprint("Pakiet ", msg.id, " jest w wierzchołku ", vertexId, " i zostało mu jeszcze ", (h - msg.ttl), " kroków do śmierci!")
			graph[vertexId].visited = append(graph[vertexId].visited, msg.id)
			msg.visited = append(msg.visited, graph[vertexId].id)

			if isTrapped == true {
				stdout <- fmt.Sprint("	<Klusownik> Przejąłem pakiet ", msg.id)
				isTrapped = false
				deadPackages++
				packages = append(packages, msg)
			} else {

				if msg.ttl == h {
					stdout <- fmt.Sprint("		Pakiet ", msg.id, " umarł!")
					deadPackages++
					packages = append(packages, msg)
				} else {

					if vertexId < n-1 {
						//If package is not in last vertex we randomize the next vertex for the packet
						i := rand.Intn(len(graph[vertexId].nexts))
						graph[vertexId].nexts[i].recChannel <- msg
					} else {
						//If package is in last vertex package is sent to channel for receiver to  avoid wrong printing
						time.Sleep(time.Second)
						receiverChan <- msg
					}
				}
			}
		default:
			continue
		}
	}

}

//Package receiver
func packageReceiver(n int) {
	var s = rand.NewSource(time.Now().UnixNano()) // seed of a pseudorandom number generator
	var r = rand.New(s)                           // generator
	for {
		select {
		//if we can get some information from channel we print it
		case msg := <-receiverChan:
			packageCounter++
			stdout <- fmt.Sprint("		Odbiorca  otrzymał: ", msg.id, " pakiet")
			packages = append(packages, msg)
		default:
			//
		}

		//if it is last package we sent information to printer to print reports
		if (packageCounter + deadPackages) == k {
			time.Sleep(time.Duration(r.Float64() * float64(time.Second)))
			finalReport <- true
		}
	}
}

func hunter() {
	var s = rand.NewSource(time.Now().UnixNano()) // seed of a pseudorandom number generator
	var r = rand.New(s)                           // generator
	for {
		vertex := r.Intn(n)
		graph[vertex].trap <- true
		stdout <- fmt.Sprint("Klusownik stawia pulapke w wierzchołku: ", vertex)
		time.Sleep(time.Duration(r.Float64()*float64(time.Second)) + 4*time.Second)
	}
}

/* --------------------------------- PRINTER ---------------------------------*/

func printer() {
	for {
		select {
		case msg := <-stdout:
			fmt.Println(msg)
		case msg := <-finalReport:
			fmt.Println(" ")
			fmt.Println(" ")
			fmt.Println("#################")
			fmt.Println("##FINAL REPORTS##")
			fmt.Println("#################")
			fmt.Println(" ")
			fmt.Println(" ")
			for i := 0; i < len(graph); i++ {
				fmt.Print("Wierzchołek ", i, " został odwiedzony przez: ")
				for j := 0; j < len(graph[i].visited); j++ {
					fmt.Print(" ", graph[i].visited[j])
				}
				fmt.Println("")
			}
			fmt.Println(" ")
			fmt.Println(" ")
			for i := 0; i < k; i++ {
				fmt.Print("Pakiet ", packages[i].id, " odwiedził wierzchołki: ")
				for j := 0; j < len(packages[i].visited); j++ {
					fmt.Print(" ", packages[i].visited[j])
				}
				fmt.Println(" ")
			}
			done <- msg
		default:
			//do nothing
		}
	}
}

