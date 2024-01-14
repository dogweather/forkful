---
title:    "Gleam recipe: Concatenating strings"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/gleam/concatenating-strings.md"
---

{{< edit_this_page >}}

## Why

String concatenation is an essential task in any programming language. It allows us to combine multiple strings into one, making it easier to manipulate and use them in our programs. In the Gleam programming language, concatenating strings is a simple and straightforward process that can greatly enhance the readability and functionality of our code.

## How To

To concatenate strings in Gleam, we use the built-in `<>` operator. Let's take a look at a simple example:

```Gleam
let first_name = "John"
let last_name = "Doe"

let full_name = first_name <> " " <> last_name

IO.println(full_name)
```

In the code above, we define two string variables `first_name` and `last_name` and then concatenate them using the `<>` operator. Finally, we print out the full name by passing the `full_name` variable to the `IO.println` function. The output of this code would be: `John Doe`.

We can also use the `<>` operator to concatenate more than two strings at once. Here's another example:

```Gleam
let message = "Welcome" <> " " <> "to" <> " " <> "Gleam!"

IO.println(message)
```

This code will output: `Welcome to Gleam!`

## Deep Dive

Under the hood, the `<>` operator in Gleam actually uses the `String.concat` function. This function takes in a list of strings as input and returns a single string that is the result of concatenating all the input strings. This means that we can also use the `String.concat` function directly to concatenate our strings. Here's the equivalent code using `String.concat`:

```Gleam
let first_name = "John"
let last_name = "Doe"

let full_name = String.concat(["John", " ", "Doe"])

IO.println(full_name)
```

Both the `<>` operator and the `String.concat` function are efficient ways of concatenating strings in Gleam. However, using the `<>` operator can make our code more readable and easier to understand.

## See Also

If you want to learn more about string concatenation in Gleam, check out the official documentation [here](https://gleam.run/book/tour/strings.html#string-interpolation). You can also explore other functions related to string manipulation in Gleam such as `String.substring` and `String.join` to further enhance your coding skills.