---
title:    "Elixir recipe: Extracting substrings"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Why
Extracting substrings is a common task in programming and is especially useful in data processing and text manipulation. In Elixir, there are built-in functions that make it easy to extract specific parts of a string or list. Learning how to use these functions can help you efficiently handle and manipulate large amounts of data.

## How To
To extract a substring in Elixir, we can use the `String.slice/3` function. This function takes in three arguments - the string or list, the starting index, and the ending index. Let's take a look at an example:
```elixir
string = "Elixir is fun to learn!"
substring = String.slice(string, 0, 6)
IO.puts(substring)
```
The output of this code would be `"Elixir"`, as we have extracted the characters from index `0` to `5`, with index `6` being non-inclusive.

We can also use negative indices to start counting from the end of the string. For example, if we wanted to extract the last 5 characters from our string, we could do so by using `-5` as the starting index:
```elixir
last_five = String.slice(string, -5, -1)
IO.puts(last_five)
```
The output here would be `"learn"`, as the last index is non-inclusive.

In addition to using `String.slice/3`, we can also use the `Enum.slice/3` function to extract a substring from any enumerable data type. Here's an example using a list:
```elixir
list = [1, 2, 3, 4, 5]
extracted = Enum.slice(list, 1, 3)
IO.inspect(extracted)
```
The output of this code would be `[2, 3, 4]`, as we have extracted the elements from index `1` to `3`, with `1` being non-inclusive.

## Deep Dive
Under the hood, the `String.slice/3` and `Enum.slice/3` functions use pattern matching and recursion to efficiently extract substrings. The starting and ending indices are used as parameters for the recursive function, which continues to call itself until the desired substring is obtained. Additionally, these functions also support list comprehensions and guards, allowing for more customized substring extraction.

There are also other handy functions such as `String.slice/4` and `Enum.slice/4` which allow for more advanced substring extraction, such as extracting every nth element from a string or list.

## See Also
- [Elixir String documentation](https://hexdocs.pm/elixir/String.html)
- [Elixir Enum documentation](https://hexdocs.pm/elixir/Enum.html#slice/3)