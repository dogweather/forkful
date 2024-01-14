---
title:    "Go recipe: Concatenating strings"
keywords: ["Go"]
---

{{< edit_this_page >}}

## Why

Strings are one of the most commonly used data types in programming, and being able to combine or "concatenate" strings together is a fundamental skill for any developer. Whether you want to create a sentence, a URL, or a message to display to a user, knowing how to concatenate strings in Go can greatly improve your coding abilities.

## How To

Concatenating strings in Go is a simple and straightforward process. Let's take a look at some coding examples and their corresponding output:

```Go
// Example 1
str1 := "Hello"
str2 := "World"
result := str1 + " " + str2

// Output
Hello World
```

In this first example, we have two separate strings, "Hello" and "World". By using the "+" operator, we can combine them together with a space in between to create the string "Hello World".

```Go
// Example 2
num := 42
str := "The answer is: "
result := str + strconv.Itoa(num)

// Output
The answer is: 42
```

In this second example, we have a number and a string. In order to concatenate them, we first need to convert the number into a string using the strconv.Itoa() function. Then, we can use the "+" operator to combine the two strings together.

```Go
// Example 3
str1 := "This is"
str2 := "a longer"
str3 := "string"
result := fmt.Sprintf("%s %s %s.", str1, str2, str3)

// Output
This is a longer string.
```

In this final example, we can also use the fmt.Sprintf() function to concatenate strings. This function allows us to create a template string with placeholders, indicated by "%s", where we can insert our separate string variables.

## Deep Dive

Now that we've seen some basic examples of how to concatenate strings in Go, let's take a deeper look at this process. Go has a built-in type called "string", which is essentially a collection of characters. When we use the "+" operator to combine strings, Go automatically creates a new string variable with the combined values of the original strings. This is called "string concatenation".

It's important to note that string concatenation in Go can only be done with two strings at a time. This means that if you have more than two strings to combine, you will need to use the "+" operator multiple times.

It's also worth mentioning that the order in which we concatenate strings matters. For example, "Hello" + "World" would create "HelloWorld", while "World" + "Hello" would create "WorldHello". Keeping this in mind can help prevent errors and ensure that your resulting string is in the correct format.

## See Also

Now that you have a better understanding of how to concatenate strings in Go, here are some additional resources that you may find helpful:

- [Go Documentation on Strings](https://golang.org/pkg/strings/)
- [Concatenating Strings in Go with Sprintf](https://www.calhoun.io/how-to-concatenate-strings-in-go/)
- [Go Playground with String Concatenation Examples](https://play.golang.org/p/KAbdDFK18Qy)

Happy coding!