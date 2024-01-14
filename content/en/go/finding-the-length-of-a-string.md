---
title:    "Go recipe: Finding the length of a string"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/go/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Why

When it comes to programming, finding the length of a string may seem like a simple task. However, it is an essential skill in order to manipulate and analyze text data in your code. Whether you are a beginner or an experienced developer, understanding how to efficiently find the length of a string in Go can greatly improve the quality and functionality of your code.

## How To

To find the length of a string in Go, we can use the `len()` function. This function takes in a string as an argument and returns an integer value representing the length of the string. Let's take a look at an example:

```
Go code:

package main

import "fmt"

func main() {
    str := "Hello World!"
    fmt.Println(len(str))
}
```

```
Output:

12
```

In this code, we have created a variable `str` with the value of "Hello World!". Using the `len()` function, we have printed out the length of that string, which is 12 characters. 

We can also use the `len()` function to find the length of a string that is passed in as an argument in a function. For example:

```
Go code:

package main

import "fmt"

func findLength(s string) {
    fmt.Println(len(s))
}

func main() {
    str := "This is a sentence."
    findLength(str)
}
```

```
Output:

19
```

This time, we have created a function `findLength` that takes in a string as a parameter. Inside the function, we use the `len()` function to find the length of the string passed in as an argument and then print it out. In our `main()` function, we have declared a string variable `str` and passed it into the `findLength` function, which prints out the length of the string.

## Deep Dive

Behind the scenes, the `len()` function in Go calculates the length of a string by counting the number of characters in the string and returning the total count. This function does not take into account the encoding or the type of characters in the string, it simply counts the number of bytes.

In Go, strings are stored as a sequence of bytes. Therefore, the length of a string is equal to the number of bytes it takes up. It's important to keep this in mind when working with strings in Go, as it may affect the outcome of certain operations.

## See Also

- [Strings in Go](https://golang.org/pkg/strings/)
- [A Tour of Go: Strings](https://tour.golang.org/basics/3)
- [Learn Go: Strings](https://www.learn-golang.org/string)