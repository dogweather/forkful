---
title:    "Haskell recipe: Searching and replacing text"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Why

Search and replace text is a common task in programming, and it allows developers to efficiently make changes to their code. In Haskell, this process can be done in a functional and concise way, making it a useful skill for any programmer to have.

## How To

To perform a simple search and replace in Haskell, we'll start by importing the necessary module:

```Haskell
import Data.Text (replace)
```

Next, we can use the `replace` function, which takes in three parameters: the old text, the new text, and the text to search and replace in. Let's say we have a string "Hello, world!" and we want to replace "world" with "Haskell":

```Haskell
replace "world" "Haskell" "Hello, world!"
```

The output of this would be "Hello, Haskell!". In this case, we're searching for the exact word "world", but we can also use pattern matching to search for a more general term within a string. For example, if we have a string "I love programming in Java" and we want to replace "Java" with "Haskell":

```Haskell
replace "Java" "Haskell" "I love programming in Java"
```

The output of this would be "I love programming in Haskell". 

We can also use the `replace` function with regular expressions to search for specific patterns within a string. For example, if we want to replace all digits in a string with an asterisk "*", we can use the following code:

```Haskell
replaceRegexInfo "[0-9]" "*" "I have 5 apples and 2 oranges."
```

The output of this would be "I have * apples and * oranges."

## Deep Dive

The `replace` function in Haskell uses the `Text` data type, which is immutable. This means that every time we perform a search and replace, a new `Text` value is created instead of modifying the original one. This is an important concept in functional programming, and it ensures that no unexpected changes are made to our code.

Additionally, the `replace` function is polymorphic, meaning it can work with different types of values, as long as they are instances of the `IsString` type class. This allows for more flexibility and allows us to search and replace in different data structures, such as lists or maps.

## See Also

- Official Haskell documentation for `Data.Text`: https://hackage.haskell.org/package/text/docs/Data-Text.html
- A beginner-friendly tutorial on using `replace` in Haskell: https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/simple-file-manipulation#the-fundamentals-replace-a-string-in-a-text