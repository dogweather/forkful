---
title:                "Elm recipe: Searching and replacing text"
simple_title:         "Searching and replacing text"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elm/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Why

As programmers, we often find ourselves needing to make changes to our code. This could be to fix bugs, improve performance, or simply make updates. One common task is searching and replacing text. This allows us to efficiently make multiple changes at once, saving us time and effort.

## How To

Searching and replacing text in Elm is a simple task. There are a few different ways to approach it, depending on your specific needs. Let's take a look at some examples using the `String.replace` function:

```
Elm 0.19.1

String.replace "world" "Elm" "Hello world!" == "Hello Elm!"

String.replace "o" "e" "Hello world!" == "Helle werld!"
```

In the first example, we are replacing the word "world" with "Elm" in the string "Hello world!". The function returns the modified string "Hello Elm!". In the second example, we are replacing the letter "o" with "e", resulting in "Helle werld!". As you can see, the `String.replace` function takes three arguments: the text to be replaced, the replacement text, and the original string.

Another way to approach searching and replacing text is using regular expressions. Elm has a regex library that we can use to search for patterns in strings. Let's take a look at an example using the `Regex.replace` function:

```
Elm 0.19.1

import Regex exposing (replace, regex)

Regex.replace (regex "\\d+") (\_ -> "number") "I have 3 apples and 5 oranges." == "I have number apples and number oranges."
```

In this example, we are using a regular expression to search for any digits in the string and replacing them with the word "number". This allows us to make changes to a string based on a specific pattern, rather than a specific text.

## Deep Dive

When searching and replacing text, it's important to pay attention to the order of the arguments in the function. The first argument is the text or pattern to be replaced, while the second argument is the replacement text. It's also worth noting that these functions are case sensitive, so "hello" and "Hello" would be considered different texts.

One way to use the `String.replace` function in a more dynamic way is by using the `words` function. This function splits a string into a list of words, making it easier to manipulate individual words. Let's take a look at an example:

```
Elm 0.19.1

words "Hello world!" == ["Hello", "world!"]
```

Now we can apply the `String.replace` function to the individual words in the list, rather than the whole string.

## See Also

- [Elm String Documentation](https://package.elm-lang.org/packages/elm/core/latest/String)
- [Elm Regex Package](https://package.elm-lang.org/packages/elm/regex/latest/)