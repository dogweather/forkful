---
title:    "Elixir recipe: Deleting characters matching a pattern"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/elixir/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Why

There are various reasons why you may need to delete characters matching a specific pattern in your Elixir code. It could be for data cleaning, removing sensitive information, or performing text processing tasks. Whatever the reason may be, knowing how to effectively delete characters matching a pattern can greatly enhance your coding skills and make your code more efficient.

## How To

To delete characters matching a pattern in Elixir, we can use the `String.replace/4` function. This function takes in four arguments: the input string, the pattern to match, the replacement string, and the number of matches to replace.

Let's see an example of how we can use this function to delete all vowels from a given string:

```Elixir
input = "Hello World!"
String.replace(input, ~r/[aeiou]/, "", global: true)
```

In this example, we first define the input string as "Hello World!", and then use the `String.replace/4` function with the pattern `~r/[aeiou]/` to match all vowels in the string. We then provide an empty string as the replacement and set the `global` option to true, indicating that we want to replace all matches.

The output of this code would be `"Hll Wrld!"` as all vowels have been successfully deleted from the string.

We can also use regular expressions to create more complex pattern matches. For example, to delete all numbers from a given string, we can use the pattern `~r/[0-9]/`.

## Deep Dive

Behind the scenes, the `String.replace/4` function uses the `Regex.replace/3` function to perform the replacement. This function takes in a regular expression as the pattern and can provide more versatile options for matching and replacing text in strings.

We can also use the `Regex.replace/3` function directly to delete characters matching a specific pattern from a string. The difference here is that we need to compile the regular expression beforehand, using the `Regex.compile/1` function.

Let's look at an example of how we can use `Regex.replace/3` to delete all lowercase letters from a string:

```Elixir
input = "Hello World!"
pattern = Regex.compile(~r/[a-z]/)
Regex.replace(pattern, input, "", global: true)
```

Similar to the previous example, we first define the input string and compile the regular expression for matching lowercase letters. Then, using the `Regex.replace/3` function, we can successfully remove all lowercase letters from the string.

## See Also

- `String.replace/4` [Elixir documentation](https://hexdocs.pm/elixir/String.html#replace/4)
- `Regex.replace/3` [Elixir documentation](https://hexdocs.pm/elixir/Regex.html#replace/3)
- [Regular expressions tutorial](https://regexone.com/)