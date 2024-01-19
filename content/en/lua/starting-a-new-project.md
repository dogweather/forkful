---
title:                "Starting a new project"
html_title:           "Bash recipe: Starting a new project"
simple_title:         "Starting a new project"
programming_language: "Lua"
category:             "Lua"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/lua/starting-a-new-project.md"
---

{{< edit_this_page >}}

## What & Why?

Starting a new project is about turning a blank slate into a meaningful codebase. Programmers initiate a new project to bring a novel idea to reality or improve existing systems.

## How To:

Here's how to create a basic Hello World program in Lua:

```lua
print("Hello, World!")
```

And the output will be:

```
Hello, World!
```

Let's kick it up a notch. Here's a Lua function for adding two numbers:

```lua
function add(a, b)
  return a + b
end

print(add(5, 3))
```

Run it, you'll see:

```
8
```
In Lua, creating a new "project" often starts with such fundamental building blocks.

## Deep Dive:

Historical Context:
Lua was born in 1993, designed to be a lightweight language for configuring and extending software, with an emphasis on embeddability.

Alternatives:
There are alternatives languages such as Python, JavaScript, Ruby, etc. However, Lua's lightweight design and speed make it the favored language for game scripting.

Implementation Details:
Lua embraces a minimalist philosophy. It aligns different lines of codes into a single thread and follows the sequential execution.

## See Also:

- For more advanced concepts, check out Lua's official documentation: [Lua 5.4 Reference Manual](https://www.lua.org/manual/5.4/).
- For more practical exercises, consider visiting [Lua-Exercises](https://exercism.io/tracks/lua).
- Looking to contribute? Join the Lua Community at [Lua-Users](http://lua-users.org/).