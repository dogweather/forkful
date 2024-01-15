---
title:                "Converting a string to lower case"
html_title:           "Elixir recipe: Converting a string to lower case"
simple_title:         "Converting a string to lower case"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Why

Converting a string to lower case is a common operation in programming, especially when dealing with user input. It allows for more flexibility in matching and comparing strings, as well as for consistent formatting of data.

## How To

```Elixir
# To convert a string to lower case, use the String.downcase/1 function:
String.downcase("HELLO WORLD") #=> "hello world"
```

You can also convert a string to lower case by pattern matching in a function:

```Elixir
def downcase_string(string) do
  _ = String.downcase(string)
end

downcase_string("HELLO WORLD") #=> "hello world"
```

## Deep Dive

Under the hood, the String.downcase/1 function uses the Unicode standard to correctly convert characters to lower case. This means that it takes into account different languages and their specific lower case rules.

In addition to String.downcase/1, Elixir also provides String.downcase/2 and String.downcase!/1 for more advanced usage. These functions allow for specifying a locale, which dictates which lower case rules to use.

## See Also

- [Elixir String Module](https://hexdocs.pm/elixir/String.html)
- [Unicode Lower Case Guidelines](https://www.unicode.org/versions/Unicode8.0.0/ch03.pdf)
- [Elixir String.downcase/1 function](https://hexdocs.pm/elixir/String.html#downcase/1)