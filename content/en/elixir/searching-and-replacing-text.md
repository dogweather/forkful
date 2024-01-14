---
title:                "Elixir recipe: Searching and replacing text"
simple_title:         "Searching and replacing text"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Why
Searching and replacing text is a common task in programming, especially when working with large amounts of data or text. In Elixir, there are various ways to perform efficient and effective text substitution, making it a useful skill to learn for any developer.

## How To
To perform a search and replace in Elixir, we can use the `String.replace/4` function. This function takes in four parameters: the initial string, the search pattern, the replacement string, and the number of replacements to make. Let's see an example of how we can use this function to replace a word in a string:

```Elixir
str = "Hello World"
String.replace(str, "World", "Elixir")
```

The output of this code would be `"Hello Elixir"`, as the word "World" has been replaced with "Elixir".

Another useful function for text substitution is `Regex.replace/3` which uses regular expressions to perform the replacement. For example, we can use it to replace all vowels in a string with an asterisk symbol:

```Elixir
str = "Hello World"
Regex.replace(~r/[aeiou]/, str, "*")
```

The output of this code would be `"H*ll* W*rld"`.

Elixir also provides `String.replace!/4` and `Regex.replace!/3` functions which will raise an error if the replacement cannot be made. These functions are useful for ensuring that the text substitution is successful.

## Deep Dive
When using `Regex.replace/3` in Elixir, it's important to note that the first parameter can either be a string or a regular expression. If it's a string, it will be converted to a regular expression using the `~r` sigil. This allows for easier and more readable regular expression operations.

It's also worth mentioning the use of the `count` parameter in `String.replace/4`. This allows us to limit the number of replacements made, which can be helpful when working with large strings.

Additionally, the `Regex.replace/4` function can also take in a function as the replacement parameter. This allows for more complex and dynamic replacements, as the function can have access to the matched parts of the string through a capture group.

## See Also
For more information on the various ways to replace text in Elixir, check out the official documentation and the following resources:

- [Official Elixir Documentation on String and Regex functions](https://hexdocs.pm/elixir/String.html#replace/4)
- [Elixir Forum Discussion on String and Regex Replace Functions](https://elixirforum.com/t/string-replace-and-regex-replace-functions/4294)
- [Elixir School Lesson on Regular Expressions](https://elixirschool.com/lessons/advanced/regular-expressions/)