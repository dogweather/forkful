---
title:                "Using associative arrays"
date:                  2024-01-30T18:57:20.804289-07:00
model:                 gpt-4-0125-preview
simple_title:         "Using associative arrays"

category:             "Go"
tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/go/using-associative-arrays.md"
---

{{< edit_this_page >}}

## What & Why?

Associative arrays, known as maps in Go, let you store and access data with key-value pairs. They are essential for managing collections where you can look up values swiftly by a unique key, simplifying data manipulation and retrieval in your programs.

## How to:

In Go, maps are straightforward to use. Here's a simple guide to kick things off:

1. **Declaring and Initializing Maps**

```Go
package main

import "fmt"

func main() {
    // Initializes an empty map with string keys and int values
    var scores map[string]int
    fmt.Println(scores) // Prints: map[]

    // Declaring and initializing a non-empty map
    colors := map[string]string{
        "red": "#ff0000",
        "green": "#00ff00",
    }
    fmt.Println(colors) // Prints: map[green:#00ff00 red:#ff0000]
}
```

2. **Adding and Accessing Elements**

```Go
func main() {
    fruits := make(map[string]int)
    fruits["apples"] = 5
    fruits["bananas"] = 10

    fmt.Println(fruits["apples"]) // Prints: 5
}
```

3. **Iterating Over Maps**

```Go
func main() {
    pets := map[string]string{"dog": "bark", "cat": "meow"}

    for key, value := range pets {
        fmt.Printf("%s goes %s\n", key, value)
    }
    // Output order may vary, as maps do not guarantee order.
}
```

4. **Deleting Elements**

```Go
func main() {
    meals := map[string]int{"breakfast": 300, "lunch": 600}
    fmt.Println(meals) // Before deletion

    delete(meals, "lunch")
    fmt.Println(meals) // After deletion
}
```

## Deep Dive

Introduced in Go 1, maps provide a built-in way to handle associative arrays efficiently. Unlike slices, which are ordered collections, maps are unordered. This means the iteration order over map elements is not guaranteed to be the same across executions, a trade-off for its ability to handle key-value pairs dynamically and with significant flexibility.

Under the hood, Go implements maps as hash tables, ensuring average complexity of access, insertion, and deletion operations is O(1), under most circumstances. However, it's worth noting that this efficiency can vary based on factors like hash collisions.

For use cases requiring ordered key traversal, you might consider combining maps with slices or exploring third-party packages that offer additional data structures like ordered maps or trees. Despite their limitations, Go's maps are a powerful and essential tool for many programming scenarios.
