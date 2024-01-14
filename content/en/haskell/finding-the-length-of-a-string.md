---
title:    "Haskell recipe: Finding the length of a string"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Why 

When working with strings in Haskell, one common task is finding the length of a string. This may seem like a simple task, but understanding how to properly find the length of a string will help with more complex string operations. 

## How To
To find the length of a string in Haskell, we can use the `length` function. This function takes in a list and returns an integer value representing the length of the list. In this case, we can use a string as our list since strings are just lists of characters in Haskell. 

Let's see an example of finding the length of a string using the `length` function:

```Haskell
length "Hello World!" 
```
Output: `12`

As you can see, the `length` function counts the number of characters in the string, including spaces and punctuation. It's important to note that the `length` function is a built-in function in Haskell, so it does not require any additional libraries to use.

If you want to find the length of a string variable, you can simply use the variable name instead of a string literal. For example:

```Haskell
let str = "This is a string"
length str
```
Output: `17`

## Deep Dive 
The `length` function may seem straightforward, but it's important to understand how it works under the hood. In Haskell, lists are composed of a head and a tail. The head is the first element in the list, and the tail is the rest of the list. When finding the length of a list, Haskell recursively counts the number of elements in the list by breaking it down into smaller sub-lists.

This is important to note because it means that the `length` function has a time complexity of O(n), where n is the length of the list. This means that as the size of the list grows, the time it takes to find the length also grows linearly.

## See Also
- [Haskell Docs: length function](https://hackage.haskell.org/package/base-4.15.0.0/docs/src/GHC.List.html#length)
- [Haskell Wiki: String](https://wiki.haskell.org/String)
- [Haskell Docs: Recursive functions](https://hackage.haskell.org/package/base-4.15.0.0/docs/Prelude.html#g:17)

By understanding how to find the length of a string in Haskell, you'll have a better understanding of how lists work and how recursive functions are implemented. Keep these concepts in mind when working with strings, and you'll be on your way to becoming a Haskell pro!