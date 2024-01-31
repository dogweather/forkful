---
title:                "Skriva tester"
date:                  2024-01-19
html_title:           "Arduino: Skriva tester"
simple_title:         "Skriva tester"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/lua/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why?
Skriva tester handlar om att verifiera koden. Programmerare gör detta för att undvika buggar och säkerställa att koden uppför sig som förväntat.

## How to:
I Lua använder vi ofta moduler som `busted` eller `luaunit` för att skriva tester. Här är ett exempel:

```Lua
-- Först installera busted via luarocks: luarocks install busted
-- fil: addition_spec.lua
describe("addition function", function()
  local addition = require "addition"

  it("sums up two numbers", function()
    assert.are.equal(5, addition(2, 3))
  end)
end)
```

Koden i `addition.lua`:

```Lua
-- fil: addition.lua
local function addition(a, b)
  return a + b
end

return addition
```

Kör dina tester med `busted addition_spec.lua`. Förväntad utdata är att testet passerar.

## Deep Dive
Testning i Lua har historiskt sett inte varit lika framträdande som i språk som Java eller Ruby, men dess vikt växer. `busted` och `luaunit` är populära testerådgiv kan inbyggda funktioner i Lua för enklare fall, men dessa bibliotek erbjuder mer omfattande funktionalitet. De hanterar allt från grundläggande assertions till mock-objekt och test fixtures.

## See Also
Djupare dykning och resurser:

- LuaUnit GitHub: [https://github.com/bluebird75/luaunit](https://github.com/bluebird75/luaunit)
- Allmän guide till testning i programmering: [https://en.wikipedia.org/wiki/Software_testing](https://en.wikipedia.org/wiki/Software_testing)
