---
title:                "Capitalizing a string"
html_title:           "Lua recipe: Capitalizing a string"
simple_title:         "Capitalizing a string"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/lua/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

Capitalizing a string means changing the first letter of each word to uppercase and all other letters to lowercase. Programmers do this for text normalization and readability, especially in user interfaces.

## How to:

Lua doesn't have a built-in function to capitalize strings. So we'll use the `gsub` function along with a pattern to achieve it. Here's an example:

```Lua
function capitalize(str)
    return (str:gsub("^%l", string.upper))
end

print(capitalize("hello, world")) -- Prints "Hello, world" 
```

In this example, `gsub` function changes the first character if it's a lowercase letter (`^%l`) to uppercase using `string.upper`.

## Deep Dive

Lua, first appeared in 1993, has been a lightweight yet versatile language for scripting. It's been flexible but the compromise often comes as lack of some built-in string manipulation functions, like capitalizing strings. But, the powerful pattern-matching functions provided in Lua easily compensate for it.

Alternate ways of capitalizing can be using ASCII values or the `utf8` library. Also note that our example only capitalizes the first letter of the string, not each word. To capitalize each word, use `"%w+"` instead of `"^%l"` in our function:

```Lua
function capitalize(str)
  return (str:gsub("%w+", function(word)
    return word:sub(1,1):upper()..word:sub(2):lower()
  end))
end

print(capitalize("hello, world"))  -- Prints "Hello, World"
```

## See Also

Learn more about Lua strings from its official documents, "Programming in Lua" is also a great resource. Also, dive deep into string patterns in this [Lua-Users Wiki](http://lua-users.org/wiki/PatternsTutorial).