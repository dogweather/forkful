---
title:                "Writing tests"
html_title:           "Arduino recipe: Writing tests"
simple_title:         "Writing tests"
programming_language: "Lua"
category:             "Lua"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/lua/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why?

Writing tests means crafting code that automatically checks your other code for mistakes. Programmers do it to catch bugs early, ensure code works as expected, and make future changes safer.

## How to:

```Lua
-- Simple Lua test example using assert

function add(a, b)
  return a + b
end

-- Test function
function testAdd()
  assert(add(2, 2) == 4)
  assert(add(-1, 1) == 0)
  print("Passed all add() tests.")
end

testAdd()  -- Running the test function
```

Output:
```
Passed all add() tests.
```

## Deep Dive

Historically, Lua didn't have a built-in testing framework, leading devs to create their own or use third-party ones like LuaUnit or busted. With a minimalist core, these frameworks handle setup/teardown, assertions, and report formats. Alternatives include using the native `assert` function for simple tests or integrating Lua with continuous integration (CI) systems for automated testing across different environments. Implementation details involve writing testable code, understanding the importance of test coverage, and designing tests that are both comprehensive and readable.

## See Also

- LuaUnit GitHub: https://github.com/bluebird75/luaunit
- busted GitHub: https://github.com/Olivine-Labs/busted
- "Programming in Lua" (testing chapter): https://www.lua.org/pil/11.html
- Lua Users Wiki on UnitTesting: http://lua-users.org/wiki/UnitTesting