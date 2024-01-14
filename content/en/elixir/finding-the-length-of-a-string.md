---
title:                "Elixir recipe: Finding the length of a string"
programming_language: "Elixir"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Why

Finding the length of a string is a fundamental task in any programming language. It allows us to determine the number of characters in a given string, which is useful when handling user input, text formatting, and data validation. In this blog post, we will explore how to find the length of a string in Elixir.

## How To

To find the length of a string in Elixir, we can use the `String.length/1` function. This function takes in a string as an argument and returns the length of the string as an integer. Let's see an example of how this function works in the code block below:

```Elixir
string = "Hello World!"
length = String.length(string)
IO.puts(length) #=> 12
```

In the code above, we assigned the string "Hello World!" to the variable `string` and then used the `String.length/1` function to find its length. The result is assigned to the variable `length`. Finally, we use the `IO.puts` function to print the result to the console.

You can also use the `String.length/1` function on strings containing unicode characters. Let's see an example of this:

```Elixir 
string = "안녕하세요"
length = String.length(string)
IO.puts(length) #=> 5
```

In this example, we have a string with Korean characters, and the `String.length/1` function still returns the correct length of 5.

## Deep Dive

Under the hood, the `String.length/1` function uses the `String.decode_utf8/1` function to decode the given string into a list of codepoints. It then counts the number of codepoints in the list and returns it as the length of the string. This approach allows the function to correctly handle strings with different character encodings.

It is also worth mentioning that strings in Elixir are represented as binaries, and the `String.length/1` function counts the number of bytes in the string. So, if you have a string with multi-byte characters, the length returned by the function may not be the same as the number of visible characters.

## See Also

- [Elixir String module documentation](https://hexdocs.pm/elixir/String.html)
- [Elixir Binary module documentation](https://hexdocs.pm/elixir/Binary.html)

Finding the length of a string may seem like a simple task, but understanding how it works under the hood will help you write more efficient and robust code. Thanks for reading!