---
title:    "Elm recipe: Finding the length of a string"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## Why

As programmers, we often encounter situations where we need to deal with strings. Whether it's user inputs, data from an API, or simple text manipulation, strings are an integral part of many applications. One common task is finding the length of a string, and in this blog post, we will explore why this is important and how to do it in Elm.

## How To

In Elm, finding the length of a string is a simple task. We can use the `String.length` function which takes a string as its argument and returns the number of characters in that string. Let's see an example:

```Elm
-- Define a string
str = "This is a sample string."

-- Use the String.length function
length = String.length str

-- Output the length
Debug.log "Length" length -- Output: Length: 23
```

In the above code, we first define a string and then use the `String.length` function to find its length. We assign the result to a variable and then output it using the `Debug.log` function. As you can see, the output is 23, which is the total number of characters in the string including spaces and punctuation.

We can also use this function on user inputs or data from an API to dynamically find the length of a string. Let's consider a case where a user enters their name and we want to show a message based on the length of their name. Here's how we can do it:

```Elm
-- Define user input
name = "John"

-- Use the String.length function
length = String.length name

-- Output message based on length
if length > 5 then 
    "Your name is longer than 5 characters!"
else 
    "Your name is 5 characters or less!"
```

In the above code, we first define the user input as a string and then use the `String.length` function to find its length. Based on the length, we output a different message to the user.

## Deep Dive

When finding the length of a string, it's important to understand how this length is calculated. In Elm, `String.length` counts the number of Unicode characters in a string, not the number of bytes. This is because in Unicode, some characters may require more than one byte to be represented. Therefore, the length of a string may vary depending on the characters it contains.

It's also worth noting that `String.length` does not take into account escape sequences such as `\n` or `\t`. It simply counts the number of characters in the given string.

## See Also

- [Official Elm documentation for String.length](https://package.elm-lang.org/packages/elm/core/latest/String#length)
- [Elm string functions](https://elmprogramming.com/string-functions.html)
- [String length in other programming languages](https://www.geeksforgeeks.org/length-function-in-c-cpp-and-java/)