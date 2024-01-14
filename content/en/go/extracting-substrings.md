---
title:    "Go recipe: Extracting substrings"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/go/extracting-substrings.md"
---

{{< edit_this_page >}}

## Why

When programming in Go, there may be times when you need to extract a substring from a larger string. This could be useful for a variety of reasons, such as parsing user input or formatting data into a specific format. In this blog post, we will explore how to extract substrings in Go and the benefits it can bring to your code.

## How To

To extract a substring in Go, we will use the built-in `substring` function from the `strings` package. This function takes in two parameters - the string you want to extract from and the indices representing the start and end of the substring.

Here is an example of using the `substring` function to extract a substring from a string variable:

```go
package main

import "fmt"
import "strings"

func main() {
  str := "Hello, world!"
  sub := strings.Substring(str, 7, 12)
  fmt.Println(sub)
}
```

The output of this code will be `world!`, as the indices 7 and 12 indicate the substring starting at index 7 and ending at index 12. Keep in mind that the indices in Go start at 0, so the first character of the string would be at index 0.

You can also use variables instead of hard-coded indices in the `substring` function. For example:

```go
package main

import "fmt"
import "strings"

func main() {
  str := "Banana"
  start := 1
  end := 4
  sub := strings.Substring(str, start, end)
  fmt.Println(sub)
}
```

The output of this code will be `ana`, as the values of the `start` and `end` variables will be used as the indices for the substring.

## Deep Dive

Behind the scenes, the `substring` function in Go utilizes a technique called "slicing" to extract the substring from the original string. Slicing is a way to create a new string from a portion of an existing string, without modifying the original string.

In the `substring` function, the slicing is done using the `[:]` notation, which specifies a range of characters to be included in the substring. For example, `str[1:4]` would extract a substring from index 1 to index 3 (remember, the end index is not included in the slice).

It's important to note that using slicing rather than creating a new string manually is more efficient, as it does not require unnecessary memory allocation and copying of data.

## See Also

If you're interested in learning more about string manipulation in Go, check out these resources:

- [A Tour of Go: Strings](https://tour.golang.org/moretypes/4)
- [The Go Programming Language Specification: Strings](https://golang.org/ref/spec#String_types)
- [String Functions in the Go Standard Library](https://golang.org/pkg/strings/)

Happy coding!