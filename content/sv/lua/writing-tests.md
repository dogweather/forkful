---
title:                "Att skriva tester"
html_title:           "Lua: Att skriva tester"
simple_title:         "Att skriva tester"
programming_language: "Lua"
category:             "Lua"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/lua/writing-tests.md"
---

{{< edit_this_page >}}

## Vad och Varför?
Att skriva tester är en viktig del av programmering. Det är ett sätt för utvecklare att säkerställa att deras kod fungerar som den ska och undvika potentiella buggar och fel.

## Hur?
För att skriva tester i Lua behöver du ett testramverk som till exempel [Luaunit](https://github.com/bluebird75/luaunit). Här är ett exempel på hur man skriver ett enkelt test:

```Lua
require("luaunit")
function test_addition()
  assertEquals(4, 2+2)
end

os.exit( LuaUnit.run() )
```
Output:
```
LuaUnit: OK.
Ran 1 test in 0.000 seconds
```

## Djupdykning
Att skriva tester är en vanlig praxis inom både agil och testdriven utveckling. Det hjälper till att upprätthålla kvaliteten på koden och möjliggör en enklare och säkrare uppdateringsprocess. Andra alternativ till Luaunit inkluderar [Busted](https://olivinelabs.com/busted/) och [Test.More](https://github.com/dcurrie/testmore).

För att implementera tester i din Lua-kod måste du först skriva funktioner som utför olika tester. Dessa funktioner kan sedan köras genom testramverket och ge utdata som visar om testerna har lyckats eller misslyckats.

## Se även
[Lua Test Patterns](http://lua-users.org/files/wiki_insecure/users/KennethLavrsen/lua_test_patterns.pdf) - En detaljerad guide för testning i Lua.

[Lua Testing Tools](https://lua-users.org/wiki/LuaTestingTools) - En lista över olika verktyg och ramverk för testning i Lua.

[Lua Official Website](https://www.lua.org/) - Den officiella websidan för Lua-programmeringsspråket.