---
title:                "Go recipe: Capitalizing a string"
simple_title:         "Capitalizing a string"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/go/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Why

In programming, proper formatting and consistency is crucial for creating clean and efficient code. One common task in string manipulation is capitalizing strings, which converts the first letter of a word to uppercase. This simple action can improve the readability and presentation of your code.

## How To

To capitalize a string in Go, we can use the `strings` package and its `Title()` function. Let's take a look at an example:

```Go
package main

import (
    "fmt"
    "strings"
)

func main() {
    // Define a string with lowercase words
    str := "hello world!"

    // Use Title() function to capitalize the first letter of each word
    fmt.Println(strings.Title(str))
}
```

Output:
```
Hello World!
```

As you can see, the `Title()` function successfully capitalized the first letter of each word in our string. This is a simple and efficient way to apply capitalization, without having to manually change the characters.

We can also use the `ToUpper()` function from the `strings` package to capitalize a specific letter in a string. Let's see an example:

```Go
package main

import (
    "fmt"
    "strings"
)

func main() {
    // Define a string with lowercase letters
    str := "hello world!"

    // Use ToUpper() function to capitalize the first letter
    fmt.Println(strings.ToUpper(string(str[0])) + str[1:])
}
```

Output:
```
Hello world!
```

In this example, we used the `ToUpper()` function to capitalize only the first letter of our string. This is useful if you want to leave the rest of the string uncapitalized.

## Deep Dive

There are a few things to keep in mind when capitalizing strings in Go. Firstly, the `Title()` function considers the beginning of a new word to be any non-letter character, such as a space or punctuation mark. This means that any non-letter character will not be capitalized, even if it appears in the middle of a word.

Additionally, keep in mind that strings in Go are immutable, meaning they cannot be changed once created. So when we use functions like `Title()` or `ToUpper()`, we are actually creating a new string with the capitalized letters, not modifying the original string.

## See Also

- [Official Go Documentation on the strings package](https://golang.org/pkg/strings/)
- [Simple ways to capitalize a string in Go](https://www.calhoun.io/how-to-capitalize-a-string-in-go/)

By following these tips and tricks, you can efficiently capitalize strings in your Go code and create a more polished end product. Happy coding!