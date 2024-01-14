---
title:    "Elm recipe: Capitalizing a string"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## Why

Have you ever needed to capitalize a string in your Elm program? Whether you're building a web application or a game, sometimes you may need to manipulate text and make it more presentable to your users. In this blog post, we'll explore how you can easily capitalize a string in Elm.

## How To

To capitalize a string in Elm, we can use the `String.toUpper` function. This function takes in a string as its argument and returns a capitalized version of that string. Let's take a look at an example:

```Elm
name = "elm programming"
capitalizedName = String.toUpper name
```

In this example, we have a variable `name` with the value of "elm programming". We then use the `String.toUpper` function to capitalize the string and assign it to a new variable called `capitalizedName`. The output of this code would be "ELM PROGRAMMING".

If you want to capitalize only the first letter of a string, we can use the `String.capitalize` function instead. Let's see it in action:

```Elm
name = "elm programming"
capitalizedName = String.capitalize name
```

The output of this code will be "Elm programming".

## Deep Dive

The `String.toUpper` and `String.capitalize` functions are convenient ways to capitalize strings in Elm. However, it's important to note that they only work with ASCII characters. If you're working with non-ASCII characters, you may run into issues. In that case, you can use the `String.toUpperAt` and `String.capitalizeAt` functions, which take in a second argument specifying the index of the character you want to capitalize.

For example:

```Elm
name = "élm programming"
capitalizedName = String.toUpperAt 0 name
```

In this code, we use the `String.toUpperAt` function to capitalize the first character of our string, which is "é". The output of this code will be "Élm programming".

It's also worth mentioning that you can use the `String.map` function to customize the capitalization of a string. This function takes in a function as its first argument, which can be used to manipulate each character in the string. For example, if we want to capitalize only the vowels in a string, we can do the following:

```Elm
name = "elm programming"
vowelCapitalizedName = String.map (\c -> if String.contains c "aeiou" then String.capitalize c else c) name
```

The output of this code will be "Elm ProgrAmmIng".

## See Also

- [The Elm-String package](https://package.elm-lang.org/packages/elm/core/latest/String)
- [Official Elm documentation on strings](https://guide.elm-lang.org/strings/)