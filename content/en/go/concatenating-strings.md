---
title:                "Go recipe: Concatenating strings"
programming_language: "Go"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/go/concatenating-strings.md"
---

{{< edit_this_page >}}

## Why
Concatenating strings is an essential skill for any programmer, especially when working with text-based data. It allows you to combine multiple strings into one, creating a larger string that contains all the information you need. This can be useful for tasks such as creating file paths, building URLs, or combining user inputs.

## How To
To concatenate strings in Go, we can use the `+` operator. Let's take a look at an example:
```Go
name := "Jane"
greeting := "Hello " + name + "!" 
fmt.Println(greeting) // Output: Hello Jane!
```
In this code, we are creating a variable `greeting` by concatenating the strings "Hello ", the value of `name` which is "Jane", and the exclamation mark. We then print out the final greeting string using `fmt.Println()`.

We can also use the `fmt.Sprintf()` function to concatenate strings. This function takes in a format string and any number of additional arguments. Let's see this in action:
```Go
age := 25
job := "programmer"
introduction := fmt.Sprintf("I am %d years old and I work as a %s.", age, job)
fmt.Println(introduction) // Output: I am 25 years old and I work as a programmer.
```
Here, we are creating a string that introduces ourselves, using the `fmt.Sprintf()` function to insert the `age` and `job` variables into our format string.

## Deep Dive
In Go, strings are immutable, meaning they cannot be changed once created. So when we use the `+` operator or the `fmt.Sprintf()` function to concatenate strings, we are actually creating a new string instead of modifying the existing ones. This is different from languages like JavaScript where strings are mutable.

To understand this better, consider the following code:
```Go
greeting := "Hello"
name := "John"
greeting += " " + name + "!"
fmt.Println(greeting) // Output: Hello John!
```
In this code, we are using the `+=` operator to concatenate the `name` variable to the end of the `greeting` variable. However, this does not actually modify the `greeting` variable, but instead creates a new string and assigns it to `greeting`.

## See Also
- Official Go documentation on strings: https://golang.org/pkg/strings/
- Tutorial on string concatenation in Go: https://www.calhoun.io/concatenating-strings-in-go/

By mastering the skill of concatenating strings, you can make your code more efficient and effective when working with text-based data. With different methods available and a solid understanding of how strings work in Go, you can easily build complex strings for various purposes. Happy coding!