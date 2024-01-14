---
title:                "Gleam recipe: Finding the length of a string"
programming_language: "Gleam"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Why

Have you ever needed to know the length of a string in your code? Perhaps you need to validate input or manipulate data. No matter the reason, knowing how to find the length of a string can be a useful skill for any programmer.

## How To

Finding the length of a string in Gleam is a simple task. First, we need to define our string using the `String.length` function, which takes the string as its argument. Let's create a string and assign it to a variable called `str`:

```Gleam
let str = "Hello World"
```

Next, we can use the `String.length` function to find the length of our string and print it to the console using the `io/format` module:

```Gleam
let length = String.length(str)
io/format("The length of the string is {}.", [length])
```

The output in our console should be:

```
The length of the string is 11.
```

Now, let's see how we can use this in a more practical scenario. For example, let's say we want to validate user input for a password that needs to be at least 8 characters long. We can use the `String.length` function to check the length of the password the user enters and display an error message if it's too short:

```Gleam
fn validate_password(password) {
  let length = String.length(password)
  if length < 8 {
    io/format("Error: Password must be at least 8 characters long.")
  } else {
    io/format("Password is valid.")
  }
}
```

Now we have a simple validation function that uses the `String.length` function to ensure the password is of sufficient length.

## Deep Dive

For those interested in a deeper understanding, the `String.length` function in Gleam is implemented using the `byte_size` function, which counts the number of bytes in a string. This is important to note because in some languages, the length of a string refers to the number of characters, not bytes. In Gleam, the `String.length` function follows the byte count approach.

## See Also

- [Gleam documentation on strings](https://gleam.run/documentation/standard-library/#strings)
- [Official Gleam website](https://gleam.run/)
- [Tutorial: Getting started with Gleam](https://emilyvm.ghost.io/getting-started-with-gleam/)

Happy coding and have fun exploring the world of Gleam!