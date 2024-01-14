---
title:                "Elm recipe: Extracting substrings"
programming_language: "Elm"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elm/extracting-substrings.md"
---

{{< edit_this_page >}}

## Why
Have you ever needed to extract a specific part of a string in your Elm code? Maybe you have a long string and only want to use a portion of it for a variable. This is where extracting substrings can come in handy. It allows you to isolate and use only the parts of the string that you need.

## How To
Extracting substrings in Elm is actually quite simple. The built-in `String.substring` function allows you to specify the start and end index of the substring you want to extract.

Let's say we have a string called `name` with the value "John Doe". If we only want to use the first name "John", we can use the `String.substring` function like this:

```Elm
name = "John Doe"
firstName = String.substring 0 4 name
```

The first parameter is the starting index and the second parameter is the ending index (not inclusive). So in this case, we start at index 0 and end at index 4, which gives us "John". We can also use negative numbers to count from the end of the string, so if we wanted to get the last name "Doe", we would use `String.substring -3 3 name`.

You can also use variables for the indices if you want more flexibility. For example, if we have a variable `index` with the value 7, we can use `String.substring index (index + 3) name` to get the three characters starting at index 7.

## Deep Dive
One important thing to note is that the `String.substring` function will return an error if the specified indices are out of range. It's always a good idea to check the length of your string before extracting a substring to avoid this error.

Another helpful function when working with substrings is `String.index`, which allows you to find the index of a specific character within a string. This can be useful if you need to extract a substring based on a certain character rather than a specific index.

For more complex substring extraction, you can also use regular expressions with the `Regex` module. This allows you to specify patterns to match and extract substrings from a string.

## See Also
- [Elm String library documentation](https://package.elm-lang.org/packages/elm/core/latest/String)
- [Elm Regex library documentation](https://package.elm-lang.org/packages/elm/regex/latest/Regex)