---
title:    "Elixir recipe: Converting a string to lower case"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Why
Converting strings to lower case may seem like a small and unimportant task in programming, but it can actually have a big impact on the readability and organization of your code. By converting strings to lower case, you ensure consistency in your code and make it easier for other developers to understand and work with your code. 

## How To
Converting a string to lower case in Elixir is a simple and straightforward process. Here's an example of how to do it using the `String.downcase/1` function:

```Elixir
string = "Hello, World!"
lowercase_string = String.downcase(string)
IO.puts lowercase_string
```
Output: hello, world!

In the above code, we use the `String.downcase/1` function to convert the `string` variable to lower case and assign it to a new variable, `lowercase_string`. We then use `IO.puts` to print out the lower case string to the console.

Another way to convert a string to lower case is by using the `String.downcase/2` function, which allows us to specify a locale for the conversion. Here's an example:

```Elixir
string = "HELLO, WORLD!"
locale = "en-US"
lowercase_string = String.downcase(string, locale)
IO.puts lowercase_string
```
Output: hello, world!

In this code, we pass in the `locale` variable as the second argument to the `String.downcase/2` function to ensure that the string is converted to lower case based on the English language rules.

## Deep Dive
When converting a string to lower case, one important thing to keep in mind is that the converted string may not always match the original string exactly. This is because certain characters may be encoded differently in upper and lower case. For example, the German character "ß" is converted to "SS" when using the `String.downcase/1` function, and may be converted to "ss" when using the `String.downcase/2` function with the "de-DE" locale.

Another thing to note is that the `String.downcase/1` function only works with ASCII characters. If you need to convert strings with non-ASCII characters to lower case, you can use the `String.downcase/2` function with the "C" locale, which allows for full UTF-8 support.

## See Also
- [String.downcase/1 function](https://hexdocs.pm/elixir/String.html#downcase/1)
- [String.downcase/2 function](https://hexdocs.pm/elixir/String.html#downcase/2)