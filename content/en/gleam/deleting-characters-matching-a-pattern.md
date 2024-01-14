---
title:    "Gleam recipe: Deleting characters matching a pattern"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Why

Have you ever wanted to quickly delete all characters in a string that match a specific pattern? Whether it's removing all vowels, spaces, or numbers, there are many use cases where deleting characters matching a pattern can be useful. In this blog post, we'll explore how to easily accomplish this task using the Gleam programming language.

## How To

To delete characters matching a pattern in Gleam, we can use the `String.delete` function. This function takes in two arguments: the string we want to modify and the pattern we want to delete. Let's take a look at a few examples:

```Gleam
my_string = "Hello, world!"

// Remove all vowels
vowels = "aeiouAEIOU"
modified_string = String.delete(my_string, vowels)
// Output: "Hll, wrld!"

// Remove all spaces
spaces = " "
modified_string = String.delete(my_string, spaces)
// Output: "Hello,world!"

// Remove all numbers
numbers = "1234567890"
modified_string = String.delete(my_string, numbers)
// Output: "Hello, world!"
```

As you can see, the `String.delete` function allows us to easily remove any characters that match a given pattern from a string. This can save us time and effort when working with large amounts of text or data.

## Deep Dive

Under the hood, the `String.delete` function uses regular expressions to match and delete characters. Regular expressions are patterns used to match and manipulate text. This means that we have the flexibility to create our own custom patterns to delete specific characters from a string. Additionally, the `String.delete` function is efficient and optimized for performance, so we don't have to worry about any slowdowns when using it.

## See Also

- [Gleam Documentation](https://gleam.run/documentation/)
- [Regular Expressions in Gleam](https://gleam.run/documentation/regular_expressions/)
- [String Functions in Gleam](https://gleam.run/documentation/strings/)