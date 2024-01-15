---
title:                "Extracting substrings"
html_title:           "Elixir recipe: Extracting substrings"
simple_title:         "Extracting substrings"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/extracting-substrings.md"
---

{{< edit_this_page >}}

## Why

Have you ever needed to extract a specific part of a string in your Elixir code? Maybe you wanted to grab a user's email address from their full name and email string or extract a specific word from a longer sentence. Whatever the reason may be, extracting substrings can be a useful tool for manipulating strings in your Elixir programs.

## How To

To extract substrings in Elixir, we can use the `String.slice/3` function. It takes in three arguments: the string we want to slice, the starting index, and the ending index. Let's see it in action with some code blocks:

```elixir
String.slice("Hello World!", 0, 5)
# Output: "Hello"

String.slice("123456789", 3, 6)
# Output: "456"
```

In the first example, we sliced the string "Hello World!" starting at index 0 and ending at index 5, which gave us the first 5 characters of the string. In the second example, we sliced the string "123456789" starting at index 3 and ending at index 6, giving us the string "456". 

But what if we want to slice from the end of the string? We can do that by using negative numbers as our indices. Let's take a look:

```elixir
String.slice("This is a sentence.", -8, -1)
# Output: "sentence"
```

Here, we sliced the string "This is a sentence." starting at the 8th character from the end and ending at the last character, giving us the word "sentence". Super convenient, right?

## Deep Dive

Now, let's dive a bit deeper into the `String.slice/3` function. The ending index we provide is not inclusive, meaning it will not include the character at that index in the final sliced string. So, if we want to slice a string and include the character at a specific index, we can simply add 1 to the ending index. Let's see an example:

```elixir
String.slice("Hello World!", 6, 12)
# Output: "World"

String.slice("Hello World!", 6, 13)
# Output: "World!"
```

In the first example, the ending index is 12, so the character at index 12 (the exclamation mark) is not included in the sliced string. In the second example, we added 1 to the ending index, and now the exclamation mark is included in the sliced string. 

Another thing to note is that if we provide an ending index that is larger than the length of the string, it will simply slice until the end of the string. Let's see an example of that:

```elixir
String.slice("Hello World!", 0, 20)
# Output: "Hello World!"
```

Even though the ending index (20) is larger than the length of the string, the function still slices until the end and returns the original string.

## See Also

- [Elixir Documentation on String.slice/3](https://hexdocs.pm/elixir/String.html#slice/3)
- [Elixir String module](https://hexdocs.pm/elixir/String.html)
- [Learn X in Y minutes - Elixir String Manipulation](https://learnxinyminutes.com/docs/elixir/#string-manipulation)