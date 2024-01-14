---
title:    "Elixir recipe: Extracting substrings"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/elixir/extracting-substrings.md"
---

{{< edit_this_page >}}

## Why

Substring extraction is a common task in programming, no matter what language you are using. It involves extracting a smaller portion of a string, also known as a substring, from a larger string. In this blog post, we will explore how to do this using Elixir.

## How To

To extract substrings in Elixir, we can use the `String.slice/3` function. This function takes three arguments: the string we want to extract from, the starting index of the substring, and the ending index of the substring. Let's look at an example:

```
Elixir code:
string = "Hello World"
substring = String.slice(string, 0, 5)

Output:
"Hello"
```

In the above code, we used `String.slice/3` to extract a substring containing the first five characters from the string "Hello World." We can also use negative numbers for the index, which will start counting from the end of the string. For example:

```
Elixir code:
string = "Hello World"
substring = String.slice(string, -5, -1)

Output:
"Worl"
```

In this code, we extracted a substring containing the last five characters from the string "Hello World."

We can also use the shorthand notation `string[start..end]` to extract substrings. Using the same examples as above, we would write:

```
Elixir code:
string = "Hello World"
substring1 = string[0..4]
substring2 = string[-5..-1]

Output:
substring1 = "Hello"
substring2 = "Worl"
```

## Deep Dive

When using `String.slice/3`, keep in mind that the ending index is not inclusive, meaning it will not be included in the extracted substring. So if we want to extract the first five characters of a string, we would use `0..4` as the range, not `0..5`.

We can also use `String.slice/2` if we want to extract a substring from a specific index until the end of the string. This function takes two arguments: the string and the starting index. For example:

```
Elixir code:
string = "Hello World"
substring = String.slice(string, 6)

Output:
"World"
```
In this code, we extracted a substring starting from index 6 until the end of the string.

We can also use `String.left/2` and `String.right/2` to extract substrings from the beginning and end of a string, respectively. These functions take two arguments: the string and the number of characters to extract. For example:

```
Elixir code:
string = "Hello World"
left_substring = String.left(string, 5)
right_substring = String.right(string, 5)

Output:
left_substring = "Hello"
right_substring ="World"
```

## See Also

To learn more about string manipulation in Elixir, check out the following resources:

- [Elixir String module documentation](https://hexdocs.pm/elixir/String.html)
- [Elixir String.slice/3 function documentation](https://hexdocs.pm/elixir/String.html#slice/3)
- [Elixir String module tutorial](https://www.sojourner.io/elegant-string-manipulation-elixir/)

Happy coding!