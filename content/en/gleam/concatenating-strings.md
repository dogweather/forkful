---
title:    "Gleam recipe: Concatenating strings"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Why

Concatenating strings is a fundamental skill in programming that allows you to combine multiple strings into a single string. This can be useful for displaying text, creating dynamic messages, and much more.

## How To

### Basic Concatenation
To concatenate strings in Gleam, we use the `<>` operator. Let's look at a simple example:

```Gleam
let first_name = "John"
let last_name = "Doe"

let full_name = first_name <> " " <> last_name

gleam_io:format(full_name)
```

Output:
```
John Doe
```

In the above code, we first declare two variables `first_name` and `last_name` with the strings "John" and "Doe" respectively. Then, we use the `<>` operator to concatenate the two strings with a space in between, and assign the result to the variable `full_name`. Finally, we use the `gleam_io` module to output the `full_name` variable.

### Concatenating with Variables
We can also use variables within our concatenation, making it more dynamic. Let's take a look at another example:

```Gleam
let first_name = "John"
let last_name = "Doe"

let name_prefix = "Mr."

let full_name = name_prefix <> " " <> first_name <> " " <> last_name
```

Output:
```
Mr. John Doe
```

In this example, we declared a new variable `name_prefix` and used it within our concatenation to add "Mr." at the beginning of the `full_name` string.

### Using Lists to Concatenate
Gleam also has a handy `gleam_list` module that allows us to work with lists of strings. We can use the `gleam_list:interleave` function to concatenate multiple strings with a separator. Let's see it in action:

```Gleam
let fruits = ["apple", "banana", "orange"]

let fruit_sentence = "I like" <> gleam_list:interleave(fruits, ",", " and")

gleam_io:format(fruit_sentence)
```

Output:
```
I like apple, banana, and orange
```

In this example, we used the `gleam_list:interleave` function to join the strings in the `fruits` list with the separator "," and an additional "and" at the end.

## Deep Dive

There are a few things to keep in mind when concatenating strings in Gleam. Firstly, keep in mind that strings in Gleam are `unicode` strings, which means they support all characters, including emojis. Secondly, the `<>` operator is just a convenient way to call the `gleam_string:concat` function, so you can use that instead if you prefer.

If you need to concatenate a large number of strings, it's recommended to use the `gomoku` or `iterable` libraries, which provide more efficient concatenation methods.

## See Also
- [Gleam String Module](https://gleam.run/docs/std-lib/string/)
- [Gleam List Module](https://gleam.run/docs/std-lib/list/)
- [Using Gomoku for efficient string concatenation](https://gleam.run/articles/concat_strings_efficiently.html)