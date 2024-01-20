---
title:                "Converting a string to lower case"
html_title:           "Clojure recipe: Converting a string to lower case"
simple_title:         "Converting a string to lower case"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/lua/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why?

String conversion to lower case is changing all the alphabetic characters in a string from upper case to lower case. Developers do this to ensure data consistency and to facilitate case-insensitive comparisons.

## How to:

Lua makes it easy to convert strings to lower case. Here is an example:

```Lua
str = "Hello, World!"
lower_str = string.lower(str)
print(lower_str)
```

Run this, and you'll see: `hello, world!` 

## Deep Dive

Firstly, case conversions are as old as ASCII, which assigned separate codes to lower-case and upper-case letters. To handle text without regard to case, humans devised lower/upper case string conversions.

In terms of alternatives, you don't have many in Lua. It's `string.lower` or manual char-to-char conversion. Needless to say, this built-in function is far easier and cleaner.

While Lua doesn't expose implementation details for `string.lower`, it likely implements a simple ASCII conversion. It'd iterate through the input and, for each upper-case ASCII character (A-Z or 65-90), minus 32 to get its lower-case equivalent (a-z or 97-122).

## See Also

For more on Lua string manipulation, visit the official Lua 5.4 manual: [https://www.lua.org/manual/5.4/manual.html#6.4](https://www.lua.org/manual/5.4/manual.html#6.4) .

If you want to know more about ASCII, this is a decent guide: [https://www.cs.cmu.edu/~pattis/15-1XX/common/handouts/ascii.html](https://www.cs.cmu.edu/~pattis/15-1XX/common/handouts/ascii.html) .