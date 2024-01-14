---
title:                "Gleam recipe: Concatenating strings"
simple_title:         "Concatenating strings"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/concatenating-strings.md"
---

{{< edit_this_page >}}

## Why

When writing a program, there will likely be times when you need to combine multiple strings together. This process is known as concatenation and it allows you to create more complex strings by combining simpler ones.

## How To

To concatenate strings in Gleam, you can use the `++` operator. This operator takes two strings as arguments and combines them together. Let's take a look at an example:

```Gleam
let first_name = "John"
let last_name = "Doe"

let full_name = first_name ++ last_name
```

In this example, the `full_name` variable will now hold the string "JohnDoe". Notice how there is no space between the first and last name, we will address this later in the deep dive section.

You can also use variables in concatenation, allowing you to create dynamic strings. For example:

```Gleam
let name = "Jane"
let greeting = "Hello "
let message = greeting ++ name

```

This will result in the `message` variable holding the string "Hello Jane".

## Deep Dive

When concatenating strings, it is important to understand how the `++` operator works. In Gleam, strings are represented as lists of characters. The `++` operator simply combines two lists together, without any additional characters or spaces. So, in order to add a space between our first and last name in the first example, we would need to explicitly add it in the code:

```Gleam
let full_name = first_name ++ " " ++ last_name
```

Another important thing to note is that the `++` operator only works on two strings. If you have more than two strings to combine, you will need to use multiple `++` operators or use the `String.concat` function.

## See Also

- Gleam String module: https://gleam.run/modules/string.html
- Glossary of Gleam terms: https://gleam.run/docs/glossary.html