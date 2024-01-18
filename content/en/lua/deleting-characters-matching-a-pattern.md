---
title:                "Deleting characters matching a pattern"
html_title:           "Lua recipe: Deleting characters matching a pattern"
simple_title:         "Deleting characters matching a pattern"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/lua/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## What & Why?

Deleting characters that match a specific pattern is a common task for programmers when working with text. It involves removing certain characters from a string that follow a particular pattern, such as all numbers or all vowels. This is often done to clean up data or perform specific text manipulation tasks.

## How to:

To delete characters matching a pattern in Lua, we can use the `gsub` function. This function takes in three parameters: the string we want to modify, the pattern we want to match, and the replacement we want to use. Here's an example:

```Lua
local str = "Hello123 World"
str = string.gsub(str, "%d", "") -- remove all numbers from the string
print(str) -- "Hello World"
```

In this example, we use the pattern "%d" to match all numbers and replace them with an empty string, effectively removing them from the original string.

We can also use more complex patterns using Lua's pattern-matching syntax. For example, if we want to remove all vowels from a string, we can use the pattern "[aeiouAEIOU]" to match all lowercase and uppercase vowels:

```Lua
local str = "Hello World"
str = string.gsub(str, "[aeiouAEIOU]", "") -- remove all vowels from the string
print(str) -- "Hll Wrld"
```

## Deep Dive:

Deleting characters matching a pattern has been a common task in programming for decades, dating back to even before the creation of Lua. Many other languages, such as Perl and Python, have their own built-in functions for pattern matching and replacement.

Instead of using Lua's `gsub` function, which takes in a string as the first parameter, we could also use the `string.find` function, which takes in a pattern as the first parameter and returns the indices of the match. We can then use the `string.sub` function to extract the parts of the string we want to keep. This approach may be preferred for more complex scenarios where we need to perform additional logic on the matched string.

When it comes to deleting characters matching a pattern, it's also important to consider the performance implications, especially when working with large strings. In some cases, using Lua patterns and built-in functions may be more efficient than using regular expressions or writing custom code. It's always best to test and benchmark different approaches to see which one is most optimal for a particular scenario.

## See Also:

If you'd like to learn more about Lua's string manipulation capabilities, you can check out the Lua documentation on [string patterns](https://www.lua.org/manual/5.4/manual.html#6.4) and [string functions](https://www.lua.org/manual/5.4/manual.html#6.4.1). You may also be interested in exploring other ways of manipulating strings, such as using [regular expressions](https://en.wikipedia.org/wiki/Regular_expression) or [string libraries](https://en.wikipedia.org/wiki/String_(computer_science)#Library).