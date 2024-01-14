---
title:    "Gleam recipe: Using regular expressions"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Why
Regular expressions are a powerful tool for manipulating and searching text in any programming language. They allow for efficient and precise pattern matching, making it easier to extract specific information from a string of characters. With Gleam, a functional programming language designed for building reliable and maintainable systems, regular expressions can be used to enhance the robustness and flexibility of your code. In this blog post, we'll explore the basics of using regular expressions in Gleam to help you understand why they are an important tool for any programmer.

## How To
To use regular expressions in Gleam, you'll need to import the `gleam/pcre` module in your code. Then, you can use the `re.match` function to search for patterns in a string. Let's say we want to check if a string contains a valid email address. We can do this using a regular expression and the `re.match` function:

```
Gleam regex = import gleam/pcre
Gleam match = regex.match("^[a-z0-9._%+-]+@[a-z0-9.-]+\.[a-z]{2,4}$", "example@email.com")

fn is_valid_email(address) {
  case match(address) {
    None -> false
    Some(_) -> true
  }
}

is_valid_email("example@email.com") // Output: true
is_valid_email("example") // Output: false
```

In the code above, we first import the `gleam/pcre` module and then use the `re.match` function to check if a given string matches the regular expression for a valid email address. Then, we define a function `is_valid_email` which takes in an email address and uses pattern matching to determine if it matches the regular expression. With regular expressions, we can easily validate email addresses or any other type of input in our code.

## Deep Dive
It's important to understand the syntax of regular expressions to effectively use them in Gleam. The `re.match` function takes in two arguments - the regular expression itself, enclosed in quotes, and the string to search for patterns in. The regular expression syntax is made up of different characters that have specific meanings. For example, `^[a-z0-9._%+-]+` matches any combination of lowercase letters, numbers, and special characters before the `@` symbol in an email address. The `+` symbol means that there can be one or more of those characters. Similarly, `@` and `[a-z0-9.-]+` match the `@` symbol and any combination of lowercase letters, numbers, and special characters after the `@` symbol. The last part, `\. [a-z]{2,4}$`, checks if the string ends with a valid domain extension.

For a more detailed look at regular expression syntax and how to use them in Gleam, you can refer to the official Gleam documentation or check out some of the resources listed in the "See Also" section below.

## See Also
- [Gleam Documentation](https://gleam.run/book/getting-started.html)
- [Regular Expressions Cheat Sheet](https://www.rexegg.com/regex-quickstart.html)
- [Regular Expressions in Gleam tutorial](https://elixirschool.com/en/lessons/advanced/regular-expressions/)
- [Gleam Reflection: Regular expressions](https://gleam.run/book/libraries-and-functions.html#regular-expressions)