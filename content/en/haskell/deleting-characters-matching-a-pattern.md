---
title:                "Haskell recipe: Deleting characters matching a pattern"
simple_title:         "Deleting characters matching a pattern"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/haskell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Why

Sometimes, when working with strings in Haskell, we may encounter the need to remove certain characters that match a specific pattern. This could be for various reasons, such as cleaning up data or formatting a string in a specific way.

## How To
To delete characters matching a pattern in Haskell, we can use the `filter` function and a lambda expression to create a predicate that will remove the desired characters. For example, to remove all vowels from a string, we can use the following code:

```Haskell
let str = "Hello world!"
let result = filter (\x -> not (x `elem` "aeiou")) str
```

The `filter` function takes in a predicate and a list, and returns a new list with only the elements that satisfy the predicate. In this case, our predicate checks if the current character is not included in the string "aeiou".

The output of this code would be: "Hll wrld!". We can see that all vowels have been removed from the original string.

## Deep Dive
The `filter` function is a higher-order function in Haskell. This means that it takes in a function as its argument. In the example above, the lambda expression `(\x -> not (x `elem` "aeiou"))` is the function that is passed to `filter`.

The function we pass to `filter` must have a type of `a -> Bool`, where `a` is the type of elements in the list. In our case, the type of characters in the string is `Char`, so our function has a type of `Char -> Bool`.

By using a lambda expression, we can create a predicate on the spot without having to define a separate function. This makes our code more concise and readable.

## See Also
- [Haskell filter function documentation](https://hackage.haskell.org/package/base/docs/Prelude.html#v:filter)
- [Lambda expressions in Haskell](https://en.wikibooks.org/wiki/Haskell/Lambda_expressions)
- [Higher-order functions in Haskell](https://en.wikibooks.org/wiki/Haskell/Higher-order_functions)