---
title:    "Elixir recipe: Searching and replacing text"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Why
Searching and replacing text is a common task in programming, and it can save you a lot of time and effort. Whether you are looking to update variable names or fix a typo, being able to quickly replace text can greatly improve your coding experience.

## How To
To search and replace text in Elixir, we will be using the built-in function `String.replace/3`. This function takes in three arguments - the original string, the pattern to be replaced, and the new replacement string. Let's take a look at some examples:

```
Elixir String.replace("Hello World", "World", "Universe")
# Output: "Hello Universe"

Elixir String.replace("Elixir is awesome!", "awesome", "amazing")
# Output: "Elixir is amazing!"

Elixir String.replace("12345", "3", "6")
# Output: "12645"
```

As you can see, the `String.replace/3` function replaces all occurrences of the pattern in the original string with the new replacement string.

## Deep Dive
It's important to note that the `String.replace/3` function only replaces the *first* instance of the pattern in the string. If we wanted to replace all occurrences, we can use the `global_replace: true` option. Let's see an example:

```
Elixir String.replace("Repeat Repeat Repeat", "Repeat", "Loop", global_replace: true)
# Output: "Loop Loop Loop"
```

Additionally, the `String.replace/3` function also supports regular expressions as the pattern argument. This allows for more advanced search and replace capabilities. Let's take a look at an example:

```
Elixir String.replace("Hello World!", ~r/[A-Z]/, "")
# Output: "ello orld!"
```

In this example, we used a regular expression to remove all uppercase letters from the string. This is just one example of how regular expressions can be used in the `String.replace/3` function.

## See Also
- [Elixir Documentation: String.replace/3](https://hexdocs.pm/elixir/String.html#replace/3)
- [Mastering Regular Expressions in Elixir](https://www.benjamintan.io/blog/2018/06/03/elixir-regular-expressions/)
- [Introduction to String Interpolation in Elixir](https://jkup.github.io/elixir-lang/string-interpolation-in-elixir/)