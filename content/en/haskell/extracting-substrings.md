---
title:    "Haskell recipe: Extracting substrings"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Why

Have you ever needed to extract a specific part of a string in your Haskell code? Maybe you wanted to manipulate it separately or use it in a different function. This is where extracting substrings comes in handy! It allows you to isolate a specific portion of a string and use it however you need.

## How To

To extract a substring in Haskell, we can use the `take` and `drop` functions. `take` takes a specific number of characters from the beginning of a string, while `drop` removes a specified number of characters from the beginning of a string. Let's see an example:

```Haskell
name = "John Doe"
first_name = take 4 name -- returns "John"
last_name = drop 5 name -- returns "Doe"
```

We can also use these functions with variables instead of explicitly giving the number of characters:

```Haskell
length_of_name = length name
initials = take 1 name ++ "." ++ take 1 (drop (length_of_name - 1) name) -- returns "J.D."
```

In the first line, we use the `length` function to get the length of `name`. Then, in the second line, we use the `take` and `drop` functions to extract the first and last initials and concatenate them with a period in between.

## Deep Dive

It's important to note that while `take` and `drop` are very useful for extracting substrings, they only work with positive indices. This means if we try to use a negative index, we will get an error. To account for this, we can use the `takeEnd` and `dropEnd` functions from the `Data.List` module. These work in the same way as `take` and `drop`, but with negative indices. Let's see an example:

```Haskell
name = "Jane Doe"
middle_name = takeEnd 3 (drop 5 name) -- returns "n"
```

We can also use the `takeWhile` and `dropWhile` functions to extract a substring based on a specific condition. These functions take a predicate function as an argument and return the portion of the string that satisfies the condition. Let's look at an example:

```Haskell
numbers = "123456789"
even_nums = takeWhile even numbers -- returns "2"
odd_nums = dropWhile even numbers -- returns "1"
```

In this example, we used the `even` predicate function from the `Data.Char` module to extract the even or odd numbers from the string.

## See Also

- [Haskell documentation for `take`](https://hackage.haskell.org/package/base-4.15.0.0/docs/Prelude.html#v:take)
- [Haskell documentation for `drop`](https://hackage.haskell.org/package/base-4.15.0.0/docs/Prelude.html#v:drop)
- [Haskell documentation for `takeEnd` and `dropEnd`](https://hackage.haskell.org/package/base-4.15.0.0/docs/Data-List.html#v:dropEnd)
- [Haskell documentation for `takeWhile` and `dropWhile`](https://hackage.haskell.org/package/base-4.15.0.0/docs/Prelude.html#v:takeWhile)