---
title:    "Gleam recipe: Finding the length of a string"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/gleam/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Why
Gleam is a modern functional programming language that has gained popularity in recent years. One of its key features is its strong type system, which makes it a great choice for building reliable and efficient applications. In this blog post, we will explore one of the most common tasks in programming - finding the length of a string, and how it can be done in Gleam.

# How To
Let's start with a simple string, "Hello, world!" and see how we can find its length in Gleam:

```Gleam
string = "Hello, world!"
length = string |> String.length
```

This will return the length of the string as an integer, in this case, 13. We can also use pattern matching to handle different cases:

```Gleam
string = ""
length = string |> String.length
```

This will return the length of an empty string as 0. We can also find the length of strings with non-ASCII characters:

```Gleam
string = "こんにちは世界"
length = string |> String.length
```

This will correctly return the length as 7, even though the string contains 10 characters.

# Deep Dive
Gleam's standard library provides the `String` module that offers various functions for working with strings. The `String.length` function is implemented using a built-in function called `Byte.length`, which returns the number of bytes in a given string.

However, this may not always correspond to the number of characters in the string, as some characters can take up more than one byte. This is where the use of Unicode comes into play. Gleam uses the UTF-8 encoding for strings, which allows it to support a wide range of characters.

When calculating the length of a string, Gleam takes into account the number of logical characters, rather than just the number of bytes. This makes it a more accurate and reliable method for finding the length of a string.

# See Also
- [Gleam Documentation](https://gleam.run/documentation/)
- [Gleam on GitHub](https://github.com/gleam-lang/gleam)

Now that you know how to find the length of a string in Gleam, you can explore more of its features and discover the potential of this powerful programming language. Happy coding!