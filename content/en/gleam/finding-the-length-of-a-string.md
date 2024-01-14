---
title:    "Gleam recipe: Finding the length of a string"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Why

String manipulation is a common task in programming, and one important aspect of string manipulation is knowing the length of a string. Being able to find the length of a string can help you perform various operations, such as checking for valid input or formatting output. In this blog post, we will explore how to find the length of a string in the Gleam programming language.

## How To

To find the length of a string in Gleam, we can use the `len` function. This function takes a string as an argument and returns the length of that string. Let's take a look at an example:

```Gleam
let string = "Hello world!"
let length = len(string)
```

In this code, we have assigned a string to a variable called `string`, and then passed that variable to the `len` function. The result of this function call is stored in a variable called `length`. We can now use this variable to perform any operations that require the length of the string.

We can also use the `len` function directly on a string literal, without the need for a variable. For example:

```Gleam
let length = len("Hello world!")
```

This will return the length of the string "Hello world!" and store it in the `length` variable.

Another way to find the length of a string is by using pattern matching. We can match on a string and use a wildcard to capture the entire string. Then, we can use the `len` function on the captured string as shown below:

```Gleam
let string = "Hello world!"
let len =
  case string {
    _string -> len(_string)
  }
```

This method can be useful in certain scenarios, such as when we need to perform different operations based on the length of a string.

## Deep Dive

Behind the scenes, the `len` function simply counts the number of characters in a string and returns that as the length. However, it is important to note that in Gleam, strings are encoded as lists of integers, where each integer represents a Unicode codepoint. This means that each character in a string can have a different number of bytes. Therefore, the length of a string can vary depending on the encoding used.

Additionally, the `len` function is not limited to working only on strings. It can also be used on tuples, lists, and arrays to return the number of elements in those data structures.

## See Also

- [Gleam documentation on strings](https://gleam.run/documentation/stdlib/string.html)
- [Pattern matching in Gleam](https://gleam.run/documentation/guide/patterns.html)
- [Unicode and encoding in Gleam](https://gleam.run/documentation/guide/unicode.html)