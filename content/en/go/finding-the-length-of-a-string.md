---
title:    "Go recipe: Finding the length of a string"
keywords: ["Go"]
---

{{< edit_this_page >}}

## Why

When it comes to programming in Go, one common task is finding the length of a string. This information can be useful in various scenarios, such as validating user input or manipulating data within a program. In this blog post, we will explore how to find the length of a string in Go and dive deeper into the process.

## How To

Finding the length of a string in Go is a simple task that can be accomplished using the built-in `len` function. Let's take a look at an example:

```Go
package main

import "fmt"

func main() {
	// defining a string variable
	str := "Hello world!"
	
	// using len function to find the length of the string
	length := len(str)
	
	// printing the length
	fmt.Println(length)
}
```

Running this code will result in the output `13` as the string "Hello world!" has 13 characters. 

We can also use the `len` function to find the length of a string stored in a variable, as shown in the example above. This function can also be used on arrays, slices, and maps, making it a versatile tool in Go programming.

## Deep Dive

When we call the `len` function, Go checks the underlying data type and returns the appropriate length. The function is implemented differently for different data types, but the underlying concept remains similar.

For strings, the length is calculated by counting the number of bytes in the string. This means that the length returned may not always correspond to the number of characters in the string since some characters may take up multiple bytes. This is important to keep in mind, especially when dealing with non-English characters.

## See Also

- [Official Go documentation on `len` function](https://golang.org/pkg/builtin/#len)
- [Tutorial on strings in Go](https://www.digitalocean.com/community/tutorials/how-to-work-with-strings-in-go)
- [Additional examples of finding string length in Go](https://golangbyexample.com/strings-length-golang-example/)

By now, you should have a good understanding of how to find the length of a string in Go and the underlying concept behind it. This is a fundamental concept in Go programming and is used in many applications. As you continue to learn and explore Go, you will undoubtedly come across situations where finding the length of a string will come in handy. Happy coding!