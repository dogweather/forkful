---
title:    "Go recipe: Capitalizing a string"
keywords: ["Go"]
---

{{< edit_this_page >}}

## Why

As a beginner in Go programming, you may have come across the need to capitalize a string. This is a common task in many applications, whether it is for data validation, displaying proper names, or simply for aesthetics. In this blog post, we will explore the reasons why you would want to capitalize a string and how to do it effectively in your Go code.

## How To

To capitalize a string in Go, we can use the built-in `strings` package which provides a `Title` function. This function takes in a string as its argument and returns the string with the first letter of each word capitalized. Let's see an example of this in action:

```
package main

import (
	"fmt"
	"strings"
)

func main() {
	name := "john doe"
	capitalizedName := strings.Title(name)
	fmt.Println(capitalizedName)
}
```

The output of this code will be `John Doe`. As you can see, the `Title` function automatically capitalized the first letter of each word in the given string.

But what if you want to capitalize just the first letter of a string, regardless of whether it is a full word or not? In that case, we can use the `ToUpper` function from the `strings` package. Let's take a look:

```
package main

import (
	"fmt"
	"strings"
)

func main() {
	name := "jane doe"
	capitalizedFirstLetter := strings.ToUpper(string(name[0])) + name[1:]
	fmt.Println(capitalizedFirstLetter)
}
```

The output of this code will be `Jane doe`. Here, we took the first letter of the string, converted it to uppercase using the `ToUpper` function, and then concatenated it with the rest of the string using the `+` operator.

## Deep Dive

In Go, strings are immutable, meaning they cannot be changed once they are created. This is why we needed to create a new string variable in the second example instead of directly modifying the original string. In fact, if we try to modify a string using indexing, we will get an error.

Another thing to keep in mind is that the `Title` function assumes a space or punctuation mark to be the start of a new word. So if you have a string like `"hello, world!"`, the `Title` function would capitalize the "w" instead of the "h". To solve this, you can use the `ToLower` function to convert the whole string to lowercase first before using `Title`.

## See Also

- [Go strings package documentation](https://golang.org/pkg/strings/)
- [Golang tutorial for beginners](https://tour.golang.org/welcome/1)
- [Effective Go: Capitalization](https://golang.org/doc/effective_go.html#capitalization)