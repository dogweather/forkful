---
title:                "Concatenating strings"
html_title:           "Haskell recipe: Concatenating strings"
simple_title:         "Concatenating strings"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/haskell/concatenating-strings.md"
---

{{< edit_this_page >}}

## Why

Concatenating strings is a common task in programming, and can be especially useful in tasks such as data manipulation and text processing. By combining multiple strings into a single string, you can create more complex and meaningful output that can be used in a variety of applications.

## How To

To concatenate strings in Haskell, we can use the `++` operator or the `concat` function. Let's take a look at some code examples and the corresponding output.

```
-- using the ++ operator
"I love" ++ " " ++ "Haskell"
-- output: "I love Haskell"

-- using the concat function
concat ["Learn", " ", "Haskell"]
-- output: "Learn Haskell"
```

We can also use these methods to concatenate more than just two strings.

```
-- using the ++ operator
"The" ++ " " ++ "quick" ++ " " ++ "brown" ++ " " ++ "fox"
-- output: "The quick brown fox"

-- using the concat function
concat ["The", " ", "lazy", " ", "dog"]
-- output: "The lazy dog"
```

We can also use variables or values inside our concatenation.

```
-- using the ++ operator
let adjective = "happy"
let noun = "puppies"
"I am" ++ " " ++ adjective ++ " to see so many " ++ noun
-- output: "I am happy to see so many puppies"

-- using the concat function
let numbers = [1, 2, 3]
concat ["The numbers", " ", show numbers, " add up to 6"]
-- output: "The numbers [1,2,3] add up to 6"
```

## Deep Dive

In Haskell, strings are represented as lists of characters. This means we can use all the list functions and operators on strings as well. For example, we can use the `map` function to transform each character in a string to another character.

```
-- using map for character transformation
map toUpper "hello" -- output: "HELLO"
map toLower "WORLD" -- output: "world"
```

We can also use the `fold` function to perform more complex operations on strings. For example, to calculate the length of a string, we can use the `foldl` function.

```
-- calculating the length of a string
let str = "Haskell rules!"
foldl (\acc _ -> acc + 1) 0 str -- output: 14
```

## See Also

- [Haskell documentation on concatenating strings](https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-String.html#g:22)
- [Learn the basics of Haskell](https://learnxinyminutes.com/docs/haskell/)