---
title:                "Elixir recipe: Converting a string to lower case"
simple_title:         "Converting a string to lower case"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elixir/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Why
As we dive into the world of Elixir programming, one common task we encounter is converting strings to lower case. This may seem like a simple task, but it can have huge implications in ensuring data consistency and compatibility within our code. In this blog post, we will explore the reasons why we may need to convert strings to lower case and how to do it in Elixir.

## How To
To convert a string to lower case in Elixir, we can use the `String.downcase/1` function. Let's see an example:

```Elixir
string = "HELLO WORLD"
lower_case_string = String.downcase(string)
```

The `String.downcase/1` function takes in a string as its argument and returns a lower case version of the string. In this case, `lower_case_string` will be equal to "hello world".

But what if we want to convert a string to lower case without creating a new variable? We can use the `String.downcase!/1` function, which modifies the string in-place.

```Elixir
mutable_string = "HELLO WORLD"
String.downcase!(mutable_string)
```

In this example, `mutable_string` will now be equal to "hello world".

Now, let's see how this works with special characters and accents.

```Elixir
string = "ÁÉÍÓÚ"
lower_case_string = String.downcase(string)
```

The `lower_case_string` will now be equal to "áéíóú". As we can see, the function also converts accented characters to their lower case equivalents.

## Deep Dive
Behind the scenes, the `String.downcase/1` function uses the `:unicode` module to convert the string to lower case. This ensures that we can handle different languages and special characters without any issues.

It is also important to note that the `String.downcase/1` and `String.downcase!/1` functions are not just limited to ASCII characters. They can handle any character in the unicode range.

## See Also
- [Elixir String documentation](https://hexdocs.pm/elixir/String.html#downcase/1)
- [Unicode module documentation](https://hexdocs.pm/elixir/Unicode.html)

In this blog post, we learned why converting strings to lower case is important and how to do it in Elixir using the `String.downcase/1` and `String.downcase!/1` functions. We also explored how these functions handle special characters and their underlying implementation using the `:unicode` module. Happy coding!