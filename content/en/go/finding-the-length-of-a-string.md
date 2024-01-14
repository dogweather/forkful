---
title:                "Go recipe: Finding the length of a string"
simple_title:         "Finding the length of a string"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/go/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Why

As developers, we often encounter tasks that require us to manipulate strings. One common task is finding the length of a string. While it may seem like a simple task, understanding how to find the length of a string in Go can greatly improve our coding efficiency and make our programs more robust.

## How To

To find the length of a string in Go, we can use the built-in `len()` function. This function takes in a string as its argument and returns the number of characters in the string. Let's take a look at an example:

```Go
package main

import "fmt"

func main() {
    str := "hello world"
    fmt.Println(len(str)) // output: 11
}
```

In the above code, we declared a string variable, `str`, and assigned it the value of "hello world". Then, we used the `len()` function to find the length of the string and printed the result to the console. As expected, the output is 11 since "hello world" has 11 characters.

We can also use the `len()` function to find the length of a string stored in a variable:

```Go
package main

import "fmt"

func main() {
    input := "How many characters am I?"
    fmt.Println(len(input)) // output: 26
}
```

Using the `len()` function makes it easy to find the length of a string, no matter how many characters it contains. However, it's important to note that this function counts the number of bytes in the string, which may not always be the same as the number of characters (e.g. when using non-English characters).

## Deep Dive

Under the hood, the `len()` function works by iterating over the string and counting each character until it reaches the end. This process is efficient and allows the function to work on strings of any length.

One thing to keep in mind is that the `len()` function only works on strings, not other data types like integers or floats. If you try to use it on a different data type, you'll get an error. Additionally, the `len()` function only counts printable characters, so any escape sequences or special characters won't be included in the count.

## See Also

- [Official Go documentation on `len()` function](https://golang.org/ref/spec#Length_and_capacity)
- [Tutorial on manipulating strings in Go](https://www.calhoun.io/6-tips-for-using-strings-in-go/)
- [Examples of using `len()` function in real-world code](https://github.com/golang/go/search?q=len&type=Issues)