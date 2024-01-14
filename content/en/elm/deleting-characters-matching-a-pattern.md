---
title:                "Elm recipe: Deleting characters matching a pattern"
simple_title:         "Deleting characters matching a pattern"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elm/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Why 
Deleting characters matching a specific pattern can be a useful task when working with strings. In this blog post, we will explore how to do this using the functional programming language, Elm. 

## How To

To delete characters matching a pattern in Elm, we can use the `filter` function in combination with some string manipulation techniques. Let's take a look at an example:

```
-- Define a function to delete characters matching a pattern
deletePattern : String -> String -> String
deletePattern pattern input =
    let
        -- Convert the pattern to a list of characters
        patternChars = String.toList pattern

        -- Filter out matching characters from the input string
        filteredString = List.filter (\c -> not <| List.member c patternChars) (String.toList input)
    in
        -- Convert the filtered list back to a string
        String.fromList filteredString
```

Here, we define a function `deletePattern` that takes in a pattern and an input string as arguments. We first convert the pattern into a list of characters, and then use the `filter` function to remove any characters in the input string that match the pattern. Finally, we convert the filtered list back into a string and return it as the output. 

Let's try running this function with some sample inputs:

```
deletePattern "aeiou" "Hello World" --> "Hll Wrld"
deletePattern "123" "1, 2, 3, go!" --> ", , , go!"
deletePattern "!?" "What?!" --> "What"
```

As you can see, the function successfully removes any characters that match the given pattern from the input string.

## Deep Dive

The `filter` function takes in a predicate function as its first argument, which determines whether or not an element should be included in the resulting list. In our example, we use the `not` function along with `List.member` to check if a character is not present in the `patternChars` list. This allows us to easily filter out any matching characters from our input string.

Additionally, we can use this same technique to delete multiple patterns from a string by combining them into a single list of characters and using the `List.member` function to check for their presence. This can be a powerful tool in data cleaning and manipulation.

## See Also
- [Elm's String module documentation](https://package.elm-lang.org/packages/elm/core/latest/String)
- [Filtering a list in Elm](https://dev.to/dalisoenovweni/filtering-a-list-in-elm-13ml)

Deleting characters matching a pattern can be a useful task in various programming scenarios. So the next time you need to clean up your string data, give this technique a try in Elm!