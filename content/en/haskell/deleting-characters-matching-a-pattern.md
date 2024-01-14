---
title:    "Haskell recipe: Deleting characters matching a pattern"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/haskell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Why

As a Haskell programmer, you may come across a situation where you need to delete characters from a string that match a certain pattern. This could be for data cleaning, text processing, or any other task where unwanted characters need to be removed. In this blog post, we will explore how to accomplish this task using Haskell.

## How To

To delete characters from a string, we will use the `filter` function in Haskell. This function takes in a predicate and a list and returns a new list with only elements that satisfy the predicate. In our case, the predicate will be a function that checks if a character matches our pattern. Let's see how this works in code:

```Haskell
inputString :: String
inputString = "abc123def456"

deleteChar :: Char -> Bool
deleteChar c = c `elem` "1234567890"

outputString :: String
outputString = filter (not . deleteChar) inputString

main :: IO ()
main = putStrLn outputString

-- Output: abcdef
```

In the above code, we have a string with some numbers and letters. Our `deleteChar` function checks if the character is a number and returns `True` if it is. We then use the `not` function to invert the result and pass it as the predicate to `filter`. This will remove all the numbers from the input string and give us the desired output.

Note that the `deleteChar` function can be modified to match any pattern that you want to delete from the string. You can use regular expressions or any other method to create the desired predicate.

## Deep Dive

The beauty of Haskell is that it provides us with powerful higher-order functions like `filter` to make such tasks easier. In the code example, we used function composition (using the `.` operator) to pass the result of `not` to `filter`. This allows us to write concise and readable code. Additionally, Haskell also has efficient data structures for working with strings, making the deletion of characters, even in large strings, a fast and efficient process.

## See Also

- [Haskell documentation for `filter`](https://hackage.haskell.org/package/base-4.14.1.0/docs/Prelude.html#v:filter)
- [Tutorial on higher-order functions in Haskell](https://www.haskell.org/tutorial/functions.html)
- [Article on efficient string manipulation in Haskell](https://jameshfisher.com/2014/11/07/efficient-string-manipulation-in-haskell/)