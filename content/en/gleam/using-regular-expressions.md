---
title:                "Gleam recipe: Using regular expressions"
simple_title:         "Using regular expressions"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Why

Regular expressions are an essential tool for any programmer, allowing for efficient and powerful string manipulation. Whether it's validating user input, searching through text, or transforming data, regular expressions can streamline your code and save you precious time. In this blog post, we'll explore how to use regular expressions in Gleam and unleash their full potential.

## How To

Using regular expressions in Gleam is simple thanks to its built-in `Regex` module. Let's look at a basic example of validating a phone number using a regular expression:

```Gleam
import gleam/regex.{match, regex}

// Define the regex pattern for a standard phone number
let phone_number_regex = regex"^(\d{3})-(\d{3})-(\d{4})$"

// Define the phone number to validate
let phone_number = "555-123-4567"

// Match the phone number against the regex
match phone_number_regex {
  Ok(_) -> println("Valid phone number")
  Error(_) -> println("Invalid phone number")
}
```

In this example, we use the `regex` function to define our pattern, which consists of three groups of three digits separated by hyphens. Then, we use the `match` function to check if our phone number string matches the regex. If it does, we print out a success message; otherwise, we print out an error message.

When running this code, we would see the output "Valid phone number," as it matches our defined pattern. However, if we were to change the phone number to "555-AB-1234," the output would be "Invalid phone number."

## Deep Dive

Regular expressions may seem daunting at first, but once you understand their syntax and patterns, they become a powerful tool in your programming arsenal. Let's take a closer look at the regular expression used in our previous example, `^(\d{3})-(\d{3})-(\d{4})$`:

- `^` and `$` are anchors, which match the start and end of a string, respectively.
- `\d` is a character class that matches any digit.
- `{3}` is a quantifier, which specifies the number of times the preceding element should occur.
- `(\d{3})` is a capturing group, which can be referenced later in the regex.
- `-` is a literal character that must match exactly.

Understanding the meaning and functionality of each of these elements allows you to create complex and precise regular expressions for your specific needs.

## See Also

- [Gleam Docs on Regular Expressions](https://gleam.run/documentation/standard-library/regex/#regular-expressions)
- [Regexr](https://regexr.com/) - A useful tool for testing and building regular expressions
- [Mastering Regular Expressions by Jeffrey E.F. Friedl](https://www.oreilly.com/library/view/mastering-regular-expressions/0596528124/) - A comprehensive guide to mastering regular expressions in any language.