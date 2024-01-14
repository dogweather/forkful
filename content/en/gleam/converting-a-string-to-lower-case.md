---
title:    "Gleam recipe: Converting a string to lower case"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Why

Converting strings to lower case is a common task in programming, especially when dealing with user input. It allows for consistent data processing and makes it easier to compare and manipulate strings. In this blog post, we will explore how to convert strings to lower case in the Gleam programming language.

## How To

To convert a string to lower case in Gleam, we can use the `String.to_lower` function. Let's look at an example using the Gleam REPL (read-eval-print loop):

```Gleam
let name = "JOHN"

// Convert to lower case
let lower = String.to_lower(name)

// Print the result
IO.println(lower)

// Output: john
```

You can see that the string "JOHN" has been converted to "john" by using the `String.to_lower` function. This function takes in a string as an argument and returns a new string in lower case.

We can also use the `String.to_lower` function in a pattern matching statement. Let's see an example:

```Gleam
let greeting = "Hi, JOHN!"

// Match the string to lower case
match String.to_lower(greeting) {
    "hi, john!" -> IO.println("Hello!")
    _ -> IO.println("Unknown greeting.")
}

// Output: Hello!
```

In this example, the `String.to_lower` function is used in the pattern to match the expected string "hi, john!". If the string matches, the `IO.println` function will print out "Hello!". If the string does not match, the statement will match the wildcard `_` and print out "Unknown greeting."

## Deep Dive

Now let's take a deeper look at how the `String.to_lower` function works. Under the hood, this function uses the `String.map` function which applies a function to each character in the string. In this case, the function used is `Char.to_lower` which converts a character to lower case. 

In addition, the `String.to_lower` function also takes into account Unicode characters. This means that it can handle more than just the basic alphabet letters. It also preserves any special characters or symbols in the string. Here's an example:

```Gleam
let emoji = "😊 GLEAM IS AWESOME!"

// Convert to lower case
let lower = String.to_lower(emoji)

// Print the result
IO.println(lower)

// Output: 😊 gleam is awesome!
```

As you can see, the emoji remains unchanged while the letters are converted to lower case. This is important to note when working with a diverse range of user input.

## See Also

- [Gleam Documentation](https://gleam.run/documentation/)
- [String functions in Gleam](https://gleam.run/documentation/stdlib/string/)
- [Unicode characters in Gleam](https://gleam.run/documentation/cheatsheet/#unicode)