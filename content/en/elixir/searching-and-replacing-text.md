---
title:                "Elixir recipe: Searching and replacing text"
programming_language: "Elixir"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Why
Are you tired of manually searching and replacing text in your code? Look no further, because Elixir has got you covered! With its powerful string manipulation functions, you can easily search and replace text in no time.

## How To
To search and replace text in Elixir, we will be using the `replace` function from the `String` module. Let's see how it works with a simple example:

```Elixir
text = "Hello, World!"
new_text = String.replace(text, "Hello", "Hi")
IO.puts new_text
```

The output of this code will be: `Hi, World!`

In this example, we used the `replace` function to find the word "Hello" in the `text` variable and replace it with "Hi". We then assigned the new string to the variable `new_text` and printed it using the `IO.puts` function.

The `replace` function also supports regular expressions. Let's see how we can use it to replace all numbers in a string with an asterisk:

```Elixir
text = "The number is 123456."
new_text = String.replace(text, ~r/[0-9]+/, "*")
IO.puts new_text
```

The output of this code will be: `The number is *.`

## Deep Dive
Elixir's `replace` function also allows us to specify the number of replacements we want to make. By default, it replaces all occurrences of the given pattern. But if we pass a `count` option, we can limit the number of replacements. Let's see an example:

```Elixir
text = "I love Elixir!"
new_text = String.replace(text, "love", "like", count: 1)
IO.puts new_text
```

The output of this code will be: `I like Elixir!`

As you can see, we only replaced the first occurrence of the word "love" with "like" because we specified a count of 1.

## See Also
- Elixir String module: https://hexdocs.pm/elixir/String.html
- String.replace documentation: https://hexdocs.pm/elixir/String.html#replace/3
- Regular expressions in Elixir: https://hexdocs.pm/elixir/regex.html

Now that you know how to search and replace text in Elixir, go ahead and give it a try in your own code! Happy coding!