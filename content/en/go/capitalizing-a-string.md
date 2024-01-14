---
title:                "Go recipe: Capitalizing a string"
programming_language: "Go"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/go/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Why
In programming, it's important to have consistent and clear formatting for strings. Capitalizing a string is a common practice for making text stand out and follow a certain style. In this blog post, we will explore how to capitalize a string using the Go programming language.

## How To
To capitalize a string in Go, we can use the strings package which provides a function called `Title()` that capitalizes the first letter of each word in a string. Let's see how this works in action:

```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	myString := "hello world"
	capitalizedString := strings.Title(myString)

	fmt.Println(capitalizedString) // output: Hello World
}
```

In the above code, we first declare a variable `myString` with the value "hello world". Then, we use the `Title()` function from the strings package to create a new string `capitalizedString` which is the same as `myString` but with the first letter of each word capitalized. Finally, we print the result to the console.

We can also use the `ToUpper()` function from the strings package to capitalize the first letter of a string. Let's take a look:

```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	myString := "hello world"
	capitalizedString := strings.ToUpper(string(myString[0])) + myString[1:]

	fmt.Println(capitalizedString) // output: Hello world
}
```

In the above code, we take the first letter of the string and use the `ToUpper()` function to convert it to uppercase. Then, we use string concatenation to join it with the rest of the string. This results in the first letter being capitalized, but the rest of the string remaining unchanged.

## Deep Dive
The `Title()` function and `ToUpper()` functions from the strings package use the Unicode standard to determine which letters should be capitalized. This means that if your string contains non-ASCII characters, they will also be capitalized according to the Unicode standard.

It's important to note that both of these functions return a new string, leaving the original string unchanged. If you want to manipulate the original string directly, you can use the `strconv` package to convert it to a byte slice and then use the `ToUpper()` or `Title()` functions.

## See Also
- [Go strings package documentation](https://golang.org/pkg/strings/)
- [Unicode standard for capitalization](https://unicode.org/reports/tr21/#Default_graphic_character_properties)

By now, you should have a good understanding of how to capitalize a string in Go. As always, don't hesitate to explore and experiment with different methods to find the one that works best for your specific use case. Happy coding!