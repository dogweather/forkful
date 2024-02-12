---
title:                "Writing tests"
aliases:
- /en/lua/writing-tests/
date:                  2024-02-03T19:03:31.526357-07:00
model:                 gpt-4-0125-preview
simple_title:         "Writing tests"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/lua/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why?

Writing tests in programming involves creating small, separate pieces of code to automatically verify that different parts of your application work as expected. For Lua programmers, testing ensures reliability and aids in maintaining code quality, speeding up the debugging process and making codebase modifications safer.

## How to:

Lua, being a lightweight yet powerful scripting language, does not include a built-in testing framework. However, third-party libraries like Busted and LuaUnit make testing relatively straightforward. Here, we'll look at examples using both.

### Using Busted

Busted is a popular Lua testing framework that offers a flexible way to write tests. Firstly, install Busted through LuaRocks (Lua's package manager) with `luarocks install busted`. Once installed, you can write your tests. Here's a simple test for a function `add` that sums two numbers:

```lua
-- add.lua
local function add(a, b)
  return a + b
end

return add
```

```lua
-- add_spec.lua
local add = require('add')

describe("Add function", function()
  it("should add two numbers correctly", function()
    assert.are.equal(5, add(2, 3))
  end)
end)
```

To run the tests, execute `busted` in your terminal. Sample output for a passing test would look like:

```
●
1 success / 0 failures / 0 errors / 0 pending : 0.002 seconds
```

### Using LuaUnit

LuaUnit is another testing framework that follows xUnit conventions and is easy to set up. Install LuaUnit via LuaRocks using `luarocks install luaunit`. Here is how you could write a similar test as above with LuaUnit:

```lua
-- add.lua remains the same

-- test_add.lua
luaunit = require('luaunit')
local add = require('add')

function testAdd()
  luaunit.assertEquals(add(2, 3), 5)
end

os.exit(luaunit.LuaUnit.run())
```

Running this script directly via Lua (`lua test_add.lua`) will output something like:

```
.
Ran 1 tests in 0.001 seconds, 1 success, 0 failures
```

Both Busted and LuaUnit offer extensive features to handle various testing scenarios, including mocking, spying, and asynchronous testing. Choosing between them lies in the specific needs of your project and your personal preference regarding syntax and functionality.
