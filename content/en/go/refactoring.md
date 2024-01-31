---
title:                "Refactoring"
date:                  2024-01-25T02:12:21.437338-07:00
model:                 gpt-4-1106-preview
simple_title:         "Refactoring"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/go/refactoring.md"
---

{{< edit_this_page >}}

## What & Why?
Refactoring is the process of restructuring existing computer code without changing its external behavior. Programmers do it to improve nonfunctional attributes of the software, like readability and maintainability, which can make the code easier to understand, reduce complexity, and help spot bugs more easily.

## How to:
Let's dive into a simple Go code refactoring example. We'll take a snippet that calculates the average of a slice of numbers and refactors it for clarity and reusability.

Original code:
```Go
package main

import "fmt"

func main() {
    numbers := []float64{8, 12, 15, 10, 7, 14}
    var sum float64
    for _, num := range numbers {
        sum += num
    }
    average := sum / float64(len(numbers))
    fmt.Println("Average:", average)
}
```

Refactored code:
```Go
package main

import "fmt"

// CalculateAverage takes a slice of float64 and returns the average.
func CalculateAverage(numbers []float64) float64 {
    sum := 0.0
    for _, num := range numbers {
        sum += num
    }
    return sum / float64(len(numbers))
}

func main() {
    numbers := []float64{8, 12, 15, 10, 7, 14}
    average := CalculateAverage(numbers)
    fmt.Println("Average:", average)
}
```

In the refactored code, we've extracted the logic that calculates the average into a separate function named `CalculateAverage`. This makes the `main` function more concise and the average calculation logic reusable and testable.

## Deep Dive
Refactoring code isn't a modern concept; it predates widespread computer use. The practice likely started in the realm of mechanical engineering or even earlier. In software, it became more formalized with the advent of object-oriented programming and extreme programming (XP) in the 1990s, notably influenced by Martin Fowler's seminal book "Refactoring: Improving the Design of Existing Code."

There are numerous refactoring techniques, from simple renaming variables for clarity to more complex patterns like extracting methods or classes. The key is to make small, incremental changes that don't modify the software's functionality but improve the internal structure.

When using Go, refactoring can be straightforward due to the language's simplicity and powerful standard library. However, it's still important to have a good set of unit tests to ensure that refactoring doesn't introduce bugs. Tools like `gorename` and `gofmt` help automate some of the processes, and IDEs often have built-in refactoring support.

Besides manual refactoring, there are a few automated code refactoring tools available for Go, such as GoLand's refactoring tools and Go Refactor. While these can speed up the process, they're not a substitute for understanding the code and making considered changes.

## See Also
 - [Refactoring in Go: Simple is Beautiful](https://go.dev/blog/slices)
 - [Effective Go: Refactoring with Interfaces](https://go.dev/doc/effective_go#interfaces)
 - [Martin Fowler's Refactoring Page](https://refactoring.com/)
 - [GoLand Refactoring Tools](https://www.jetbrains.com/go/features/refactorings/)
