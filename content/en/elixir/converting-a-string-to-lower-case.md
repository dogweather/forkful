---
title:    "Elixir recipe: Converting a string to lower case"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/elixir/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Why
Converting strings to lowercase is a common task in programming, especially when working with user input or manipulating text data. In Elixir, there are a few different ways to accomplish this, each with its own strengths and drawbacks. So let's dive into how to convert strings to lowercase in Elixir.

## How To
There are two main ways to convert a string to lowercase in Elixir: using the `String.downcase/1` function or accessing the `String.downcase/2` macro directly.

### Using String.downcase/1
The `String.downcase/1` function takes in a string as an argument and returns a new string with all characters converted to lowercase. For example:
```Elixir
String.downcase("HELLO THERE")
# => "hello there"
```

### Using String.downcase/2
The `String.downcase/2` macro works similarly to `String.downcase/1`, but also includes an extra argument for specifying a locale. This can be useful when dealing with non-English characters. Here's an example:
```Elixir
String.downcase("HÉLLO THERE", "en-US")
# => "héllo there"
```

### Converting a string in place
If you need to convert a string to lowercase in place, without creating a new string, you can use the `String.downcase!/1` function. This function modifies the original string and returns `:ok` if successful. Otherwise, it raises an error.

## Deep Dive
Internally, `String.downcase/1` and `String.downcase/2` both use the `String.Unicode.downcase/1` function to perform the actual conversion. This function uses the Unicode standard to make sure all characters are correctly converted to lowercase, including those with accents or diacritics.

It's worth noting that `String.downcase/1` and `String.downcase/2` both return new string objects, rather than modifying the original string in place. This means that if you need to convert multiple strings to lowercase, using these functions can potentially create a large number of new string objects and impact performance.

## See Also
- [Elixir String documentation](https://hexdocs.pm/elixir/String.html)
- [Unicode case mapping in Elixir](https://hexdocs.pm/elixir/String.Unicode.html)