---
date: 2024-01-20 17:42:42.361483-07:00
description: "Deleting characters matching a pattern in Lua is about using patterns\
  \ to identify and remove specific sequences of characters from a string. Programmers\u2026"
lastmod: '2024-03-13T22:45:00.191112-06:00'
model: gpt-4-1106-preview
summary: "Deleting characters matching a pattern in Lua is about using patterns to\
  \ identify and remove specific sequences of characters from a string. Programmers\u2026"
title: Deleting characters matching a pattern
---

{{< edit_this_page >}}

## What & Why?

Deleting characters matching a pattern in Lua is about using patterns to identify and remove specific sequences of characters from a string. Programmers do this for tasks like cleaning up input, removing unwanted data, or pre-processing text for further operations.

## How to:

In Lua, we can use the `gsub` function to replace occurrences of a pattern with another string – an empty string when we want to delete them:

```lua
local text = "Hello, 123 World! 456"
local pattern = "%d" -- pattern that matches all digits
local cleanedText, numOfReplacements = text:gsub(pattern, "")

print(cleanedText) -- Output: "Hello,  World!"
print("Number of replacements made:", numOfReplacements) -- Output: "Number of replacements made: 6"
```

Notice that `gsub` also returns the number of replacements made, which can be handy information.

## Deep Dive

Lua patterns are simpler than regular expressions found in other languages but are still powerful. Historically, Lua's decision to implement a lighter pattern-matching mechanism is rooted in keeping the language both lightweight and fast.

Alternatives include using loops with `string.find` and `string.sub` to manually inspect and manipulate strings, but this is generally less efficient than pattern matching with `gsub`.

Implementation-wise, when `gsub` is called with a pattern, Lua internally compiles this pattern into a bytecode which is then executed by the pattern matcher. It's worth noting that there's a distinction between Lua patterns and true regular expressions, with the former having a smaller feature set which excludes constructs like look-aheads or back-references.

## See Also

- Lua 5.4 Reference Manual for `string.gsub`: https://www.lua.org/manual/5.4/manual.html#pdf-string.gsub
- Programming in Lua (first edition) available online for understanding patterns: https://www.lua.org/pil/20.2.html
- An online Lua pattern tester to experiment with Lua's pattern matching: https://www.lua.org/cgi-bin/demo

Remember, these tools will help solidify your understanding of Lua's pattern matching and give you a sandbox to test your string manipulations. Happy coding!
