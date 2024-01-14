---
title:    "Go recipe: Concatenating strings"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/go/concatenating-strings.md"
---

{{< edit_this_page >}}

## Why
Have you ever wanted to combine multiple smaller strings into one in your Go program? Perhaps you need to display a full name or construct a complex error message. Whatever the reason, concatenating strings is a useful skill to have in your coding arsenal.

## How To
To concatenate strings in Go, we can use the `+` operator or the `fmt.Sprintf()` function. Let's take a look at these in action:

```
Go

// Using the + operator
firstName := "John"
lastName := "Smith"

fullName := firstName + " " + lastName
fmt.Println(fullName) // Output: John Smith

// Using fmt.Sprintf()
message := fmt.Sprintf("Hello %s, it's nice to meet you!", firstName)
fmt.Println(message) // Output: Hello John, it's nice to meet you!
```

In the first example, we simply use the `+` operator to combine the `firstName` and `lastName` variables with a space in between. In the second example, we use `fmt.Sprintf()` which takes in a format string and arguments. The first `%s` represents where the `firstName` variable will be inserted in the string.

But what if we need to concatenate more than just two strings? We can use `strings.Join()` to simplify our code and make it more efficient:

```
Go

// Using strings.Join()
words := []string{"Hello", "world", "how", "are", "you?"}
message := strings.Join(words, " ")

fmt.Println(message) // Output: Hello world how are you?
```

By passing in the slice `words` and the space character as arguments, `strings.Join()` will concatenate all the strings within the slice with the space between each element.

## Deep Dive
Behind the scenes, when we use the `+` operator to concatenate strings, Go is actually creating a new string that combines the two original strings. This can be inefficient if we are concatenating many strings in a loop. On the other hand, when using `fmt.Sprintf()` or `strings.Join()`, Go is using the `strings.Builder` type which is optimized for string concatenation. This means it is more efficient and faster when concatenating multiple strings.

Another thing to keep in mind is that strings in Go are immutable, meaning they cannot be changed once created. So when we concatenate strings, we are technically creating a new string every time. This is why using `strings.Builder` or `string.Join()` is recommended as they can handle the creation and modification of strings more efficiently.

## See Also
- [Golang.org tutorial on strings](https://golang.org/doc/tutorial/strings)
- [Go by Example: String Formatting](https://gobyexample.com/string-formatting)
- [Effective Go: Strings](https://golang.org/doc/effective_go#strings)