---
title:                "Elixir recipe: Converting a string to lower case"
programming_language: "Elixir"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Why

Have you ever needed to convert a string to lower case in Elixir? Whether you're working with user input or manipulating data, converting strings to lower case can be a useful skill to have. In this blog post, we'll take a look at why you might want to do this and how to accomplish it in Elixir.

## How To

Converting a string to lower case in Elixir is simple thanks to the `String.downcase/1` function. Let's take a look at some examples below:

```Elixir
string = "HELLO WORLD"
String.downcase(string)
# Output: "hello world"

another_string = "eLiXir iS fUn!"
String.downcase(another_string)
# Output: "elixir is fun!"
```

As you can see, the `String.downcase/1` function takes in a string as an argument and returns the string in all lowercase letters.

However, it's important to note that this function only works for ASCII characters. If you need to handle Unicode characters, you can use the `String.downcase/2` function and pass in the `:ascii` option. Let's take a look at an example:

```Elixir
string = "ÉLIXIR"
String.downcase(string, :ascii)
# Output: "éllixir" 
```

You can also use the `String.downcase/1` function on a string that contains multiple words. Let's see how it works:

```Elixir
string = "Hello World"
String.downcase(string)
# Output: "hello world"
```

## Deep Dive

Now, let's dive a bit deeper into how the `String.downcase/1` function works. Behind the scenes, this function is actually using the `String.to_lower/1` function. This function takes in a char list and returns a new list with all the characters converted to lower case.

It's important to understand that Elixir strings are represented as lists of integers, with each integer representing a character in the string. Here's an example of how to manually convert a string to lower case using the `String.to_lower/1` function:

```Elixir
string = "UPPER"
lower_string = String.to_lower([string])
# Output: [117, 112, 112, 101, 114] 
```

As you can see, the characters in the original string have been converted to their corresponding integer values. You can use the `Enum.map/2` function to convert the list of integers back into a string:

```Elixir
lower_string |> Enum.map(&([&1])) |> Enum.join()
# Output: "upper"
```

## See Also

- Official Elixir documentation on `String.downcase/1`: https://hexdocs.pm/elixir/String.html#downcase/1
- Elixir School tutorial on Strings: https://elixirschool.com/lessons/basics/strings/