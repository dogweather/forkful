---
title:    "Haskell recipe: Concatenating strings"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Why 

Concatenating strings is an essential task in any programming language, and Haskell is no exception. String concatenation allows you to combine multiple strings together to form a larger string, making it useful for creating messages, formatting text, or building complex data structures.

## How To 

To concatenate strings in Haskell, you can use the `++` operator. Let's look at an example:

```Haskell
concatStrings :: String -> String -> String
concatStrings str1 str2 = str1 ++ str2

main = do
  let greeting = "Hello, "
  let name = "John"
  let message = concatStrings greeting name
  putStrLn message
```

Running this code will result in the following output:

```
Hello, John
```

In this example, we first declared a function called `concatStrings` that takes in two string parameters and uses the `++` operator to concatenate them. Then, in our `main` function, we declared two variables with strings as their values. We then used our newly created function to combine these two strings, and finally printed the result using the `putStrLn` function.

You can also use the `concat` function from the `Data.List` module to concatenate a list of strings. Let's see how this works in code:

```Haskell
import Data.List

names :: [String]
names = ["John", "Jane", "Adam"]

greeting :: String
greeting = "Hello, "

message :: String
message = concat [greeting, concat names]

main = do
  putStrLn message
```

The output of this code will be:

```
Hello, JohnJaneAdam
```

In this example, we first imported the `Data.List` module to gain access to the `concat` function. Then, we created a list of strings called `names` and declared two strings, `greeting` and `message`. Using the `concat` function, we combined the `greeting` string with the `names` list, resulting in a single string that we then printed using the `putStrLn` function.

## Deep Dive 

In Haskell, strings are simply lists of characters, which explains why we were able to use the `concat` function to combine multiple strings. However, when using the `++` operator, Haskell checks the entire length of the first string before appending the second string, which can be inefficient for large strings. To avoid this, you can use the `Text` data type from the `Data.Text` module. The `Text` data type provides much faster string manipulation compared to the standard `String` type.

## See Also 

- [Haskell String Manipulation](https://www.tutorialspoint.com/haskell_data_types/haskell_strings.htm)
- [Real World Haskell: Strings](http://book.realworldhaskell.org/read/strings.html)
- [Haskell Documentation: Data.List](https://www.haskell.org/hoogle/?hoogle=Data.List&scope=set:simple)