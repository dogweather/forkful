---
title:                "Elm recipe: Deleting characters matching a pattern"
programming_language: "Elm"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elm/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Why

Have you ever needed to delete certain characters from a string in your Elm program? Maybe you want to remove all punctuation or numbers from a sentence. Or maybe you want to clean up user input before processing it further. Whatever the reason, deleting characters matching a pattern can be a useful task to know how to do in the Elm programming language.

## How To

To delete characters that match a certain pattern, we can use Elm's `String.filter` function. This function takes in a predicate function and a string, and returns a new string with only the characters that pass the predicate.

This may sound a bit confusing, so let's look at an example. Say we have a string that contains the phrase "hello world! My name is Elm!". We want to remove all the exclamation points from this string.

```Elm
import String exposing (filter)

string = "hello world! My name is Elm!"
filteredString = String.filter (\char -> char /= '!') string
```

The `String.filter` function takes in a predicate function (`\char -> char /= '!'`) and applies it to each character in the string. In this case, we are saying that we want to keep all characters except for exclamation points. The resulting `filteredString` will be "hello world My name is Elm".

We can also use regular expressions with the `String.filter` function to delete characters that match a specific pattern. Let's say we want to remove all numbers from a string:

```Elm
import String exposing (filter)

string = "I have 3 cats and 2 dogs"
filteredString = String.filter (\char -> not (String.isDigit char)) string
```

The `String.isDigit` function checks if a character is a number, and the negation (`not`) ensures that it only filters out numbers and keeps all other characters. The resulting `filteredString` will be "I have cats and dogs".

## Deep Dive

Behind the scenes, the `String.filter` function is using a recursive technique called "filtering with an accumulator". This means that it keeps track of the filtered characters in an accumulated value, and then returns the final result once all characters have been checked.

In our first example, the accumulator starts off as an empty string. The predicate function checks each character in the original string and adds it to the accumulator if it does not match the pattern. Once all characters have been checked, the accumulator becomes the `filteredString` that we see as the final result.

In our second example, the accumulator starts off as an empty string as well. The predicate function checks each character and only adds it to the accumulator if it is not a number. The final result is a string without any numbers.

## See Also

- [Elm String docs](https://package.elm-lang.org/packages/elm/core/latest/String)
- [Elm Regular Expressions](https://package.elm-lang.org/packages/elm/regex/latest)

Deleting characters from a string based on a pattern is just one of the many tasks you can accomplish with the powerful `String` functions in Elm. With a solid understanding of these functions, you can confidently manipulate and transform strings in your programs. So give it a try and see what other cool things you can do with Elm!