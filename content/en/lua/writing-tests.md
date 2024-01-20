---
title:                "Writing tests"
html_title:           "Lua recipe: Writing tests"
simple_title:         "Writing tests"
programming_language: "Lua"
category:             "Lua"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/lua/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why?

Writing tests is the process of creating small programs that verify if a larger program is functioning correctly. Programmers do this to catch any errors or bugs in their code, ensure that modifications to the code do not break existing functionality, and improve the overall quality of their program.

## How to:

To write tests in Lua, we will be using a unit testing framework called "LuaUnit." First, install LuaUnit using LuaRocks by running the following command in your terminal:

```Lua
luarocks install luaunit
```

Next, create a new Lua file and import the LuaUnit framework by adding this line at the top:

```Lua
local lu = require('luaunit')
```

Now, let's create a simple test case that checks if the function `add` correctly adds two numbers:

```Lua
function testAdd()
    assertEquals(add(2, 3), 5)
end
```

We use the `assertEquals` function provided by LuaUnit to compare the result of `add(2, 3)` with the expected value of `5`. If the two values are not equal, the test will fail.

To run our test, we call the `LuaUnit.run()` function, passing in the name of our test function:

```Lua
LuaUnit.run(testAdd)
```

If all goes well, we should see an output stating that our test passed. Great job, we just wrote our first test in Lua!

## Deep Dive:

Unit testing has been around for a long time and is widely used to help developers catch bugs and improve code quality. In the past, testing frameworks for Lua were not as robust or popular as they are now, but with the rise of Lua's popularity and the development of tools like LuaUnit, writing tests in Lua has become much easier and accessible.

Apart from LuaUnit, there are several other testing frameworks available for Lua, such as busted, telescope, and Lutest. Each has its own unique features and syntax, and it is up to the programmer to choose the one that best suits their needs.

Writing tests in Lua often involves breaking down a larger program into smaller, testable units. These units are often functions or modules, which are then tested individually to ensure they function correctly.

## See Also:

- [LuaUnit GitHub repository](https://github.com/bluebird75/luaunit)
- [LuaRocks website](https://luarocks.org/)
- [Telescope testing framework](https://github.com/norman/telescope)