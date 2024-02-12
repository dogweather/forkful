---
title:                "Concatenating strings"
aliases:
- /en/elixir/concatenating-strings.md
date:                  2024-01-27T10:38:42.803142-07:00
model:                 gpt-4-0125-preview
simple_title:         "Concatenating strings"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/concatenating-strings.md"
---

{{< edit_this_page >}}

## What & Why?
Concatenating strings is about joining two or more strings together to form a single piece of text. You might need to merge text for generating user messages, creating file paths, or for data serialization processes. It’s a fundamental operation in any programming language, including Elixir, enabling developers to construct dynamic strings with ease.

## How to:
In Elixir, you can concatenate strings in a few straightforward ways. Let's explore the most common methods:

1. Using the `<>` operator, which is the simplest and most direct way to concatenate strings:

```elixir
name = "Jane"
greeting = "Hello, " <> name <> "!"
IO.puts greeting
# Output: Hello, Jane!
```

2. Using interpolation for clearer syntax, especially handy when you want to inject variables into a string:

```elixir
name = "John"
age = 28
introduction = "My name is #{name} and I am #{age} years old."
IO.puts introduction
# Output: My name is John and I am 28 years old.
```

3. Concatenating lists of strings with the `Enum.join/2` function:

```elixir
parts = ["Elixir", " is", " awesome!"]
message = Enum.join(parts)
IO.puts message
# Output: Elixir is awesome!
```

Remember, each method has its context where it shines, so choose according to your needs.

## Deep Dive
String concatenation in Elixir, like in many functional languages, is not without its nuances. Due to Elixir's immutable nature, every time you concatenate strings, you're actually creating a new string. This might lead to performance implications for highly iterative operations, something languages like C or Java might manage more efficiently due to mutable strings or specialized buffers.

Historically, developers have come up with various strategies to handle string concatenation efficiently in functional languages. For instance, using lists to accumulate strings and only performing the concatenation operation at the very last moment is a common pattern. This approach takes advantage of the way lists are implemented in Erlang (the underlying runtime system for Elixir) for more efficient memory usage.

Elixir provides the `IOList` as an alternative, allowing you to efficiently generate large amounts of text without the intermediate strings you'd get from repeated concatenation. An IOList is essentially a nested list of strings or character codes that the BEAM (Erlang's virtual machine) can write directly to an output, like a file or the network, without gluing them together first.

```elixir
content = ["Header", "\n", "Body text", "\n", "Footer"]
:ok = File.write("example.txt", content)
```

In this snippet, `content` is an IOList, and we write it to a file directly. This kind of operation would be both less readable and less efficient if done by repeatedly concatenating strings to construct the entire file content in memory first.

Understanding these underlying concepts and tools can significantly improve your efficiency and performance when dealing with string operations in Elixir.

## See Also
For more in-depth reading on strings and performance in Elixir, the following resources will be beneficial:

- [Elixir's Official Guide on Binaries, Strings, and Charlists](https://elixir-lang.org/getting-started/binaries-strings-and-char-lists.html)
- [Erlang Efficiency Guide](http://erlang.org/doc/efficiency_guide/listHandling.html) - While tailored to Erlang, much of this applies to Elixir due to its foundation on the Erlang VM.
