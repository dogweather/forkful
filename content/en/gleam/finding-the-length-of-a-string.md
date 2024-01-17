---
title:                "Finding the length of a string"
html_title:           "Gleam recipe: Finding the length of a string"
simple_title:         "Finding the length of a string"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# What & Why?

Finding the length of a string is the process of determining the number of characters in a given string. This is a common task in programming, as it allows us to manipulate and work with strings in more efficient ways. By knowing the length of a string, we can access specific characters and perform various operations on the string.

# How to:
To find the length of a string in Gleam, we can use the built-in function ```Gleam.String.length()```. Let's look at an example of how this works:

```Gleam
let my_string = "Hello, world!"
let length = Gleam.String.length(my_string)
```

In this example, we declare a string variable ```my_string``` with the value "Hello, world!" and then call the ```length()``` function on it, assigning the result to a new variable ```length```. The ```length``` variable will now contain the value 13.

# Deep Dive
## Historical Context
The concept of strings and string manipulation has been around since the early days of computer programming. In the past, determining the length of a string was a more complex and time-consuming task. However, with the advancement of programming languages and the creation of built-in string functions, finding the length of a string has become a much more straightforward and efficient process.

## Alternatives
In Gleam, we can use a variety of other functions to work with strings, such as ```substring()```, ```concat()```, and ```replace()```. These functions allow us to manipulate and modify strings in different ways, depending on our specific needs.

## Implementation Details
The ```Gleam.String.length()``` function works by using a looping mechanism to iterate through the characters in the string and count them until the end of the string is reached. This is done behind the scenes and allows us to get the length of the string in a single line of code.

# See Also
To learn more about Gleam and its string manipulation capabilities, check out the official documentation here: https://gleam.run/.

For a more in-depth understanding of strings and string manipulation in programming, you can refer to this article from W3Schools: https://www.w3schools.com/python/python_strings.asp.