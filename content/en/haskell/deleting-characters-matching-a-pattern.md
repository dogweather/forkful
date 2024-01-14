---
title:    "Haskell recipe: Deleting characters matching a pattern"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Why 

Have you ever found yourself in a situation where you have a string and you want to delete all characters that match a certain pattern? Perhaps you want to filter out all vowels from a word, remove all punctuation from a sentence, or delete certain digits from a number. Whatever your reason may be, learning how to delete characters matching a pattern in Haskell can greatly improve your string manipulation skills.

## How To 

To begin, let's define a simple string that we will use for our examples:

```Haskell
myString = "Hello, Haskell!"
```

### Deleting all vowels

To delete all vowels from our string, we can use the `filter` function along with the `notElem` function to check if a character is not in a given list. In this case, our list will contain all vowels. Here's the code:

```Haskell
filteredString = filter (`notElem` "aeiouAEIOU") myString 
```

The backticks around `notElem` indicate that it is an infix function, which allows us to use it in a more readable way. The output will be:

```Haskell 
filteredString = "Hll, Hskll!" 
```

### Removing all punctuation 

To remove all punctuation from our string, we can use the `filter` function along with the `isPunctuation` function from the `Data.Char` module. This function will check if a character is a punctuation mark. Here's the code:

```Haskell 
import Data.Char (isPunctuation)

filteredString = filter (not . isPunctuation) myString 
```

The `not` function is necessary because `filter` expects a predicate that returns a boolean value. The output will be:

```Haskell 
filteredString = "Hello Haskell" 
```

### Deleting certain digits 

To delete certain digits from a number, we can convert it to a string and then use the `filter` function along with the `notElem` function to check if a character is not in a given list. Here's the code:

```Haskell 
myNumber = 123456789
filteredNumber = filter (`notElem` "1357") (show myNumber) 
```

The `show` function converts our number to a string, and then we use the same technique as before to filter out certain characters. The output will be:

```Haskell 
filteredNumber = "2468" 
```

## Deep Dive 

In Haskell, strings are simply lists of characters, which allows us to use list manipulation functions like `filter` on them. The `filter` function takes two arguments: a predicate function and a list. It then returns a new list containing all the elements of the original list that satisfy the predicate. In our examples, we used `notElem` to check if a character is not in a given list, but we could also use other predicate functions like `isLower`, `isUpper`, or `isDigit` to filter our characters. 

## See Also 

For more information on string manipulation in Haskell, check out the following resources:

- [Haskell Strings Tutorial](https://www.tutorialspoint.com/haskell/haskell_strings.htm)
- [Haskell Standard Library](https://www.haskell.org/onlinereport/standard-prelude.html#g:18)
- [Hackage documentation on the `Data.Char` module](https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-Char.html)