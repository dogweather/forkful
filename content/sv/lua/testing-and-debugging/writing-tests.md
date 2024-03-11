---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:31:37.078534-07:00
description: "Att skriva tester i programmering inneb\xE4r att skapa sm\xE5, separata\
  \ kodstycken f\xF6r att automatiskt verifiera att olika delar av din applikation\
  \ fungerar som\u2026"
lastmod: '2024-03-11T00:14:11.418594-06:00'
model: gpt-4-0125-preview
summary: "Att skriva tester i programmering inneb\xE4r att skapa sm\xE5, separata\
  \ kodstycken f\xF6r att automatiskt verifiera att olika delar av din applikation\
  \ fungerar som\u2026"
title: Skriva tester
---

{{< edit_this_page >}}

## Vad & Varför?

Att skriva tester i programmering innebär att skapa små, separata kodstycken för att automatiskt verifiera att olika delar av din applikation fungerar som förväntat. För Lua-programmerare säkerställer testning tillförlitlighet och hjälper till med att underhålla kodkvaliteten, påskyndar felsökningsprocessen och gör ändringar i kodbasen säkrare.

## Hur man gör:

Lua, som är ett lättviktigt men kraftfullt skriptspråk, inkluderar inte ett inbyggt testramverk. Dock gör tredjepartsbibliotek som Busted och LuaUnit testning relativt enkel. Här kommer vi att titta på exempel som använder båda.

### Använda Busted

Busted är ett populärt Lua-testramverk som erbjuder ett flexibelt sätt att skriva tester. Först, installera Busted genom LuaRocks (Luas pakethanterare) med `luarocks install busted`. När det är installerat kan du skriva dina tester. Här är ett enkelt test för en funktion `add` som summerar två tal:

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

describe("Addfunktionen", function()
  it("ska korrekt addera två tal", function()
    assert.are.equal(5, add(2, 3))
  end)
end)
```

För att köra testerna, exekvera `busted` i din terminal. Exempelutmatning för ett lyckat test kan se ut så här:

```
●
1 framgång / 0 misslyckanden / 0 fel / 0 väntande : 0.002 sekunder
```

### Använda LuaUnit

LuaUnit är ett annat testramverk som följer xUnit-konventioner och är lätt att sätta upp. Installera LuaUnit via LuaRocks med `luarocks install luaunit`. Så här kan du skriva ett liknande test som ovan med LuaUnit:

```lua
-- add.lua är densamma

-- test_add.lua
luaunit = require('luaunit')
local add = require('add')

function testAdd()
  luaunit.assertEquals(add(2, 3), 5)
end

os.exit(luaunit.LuaUnit.run())
```

Att köra detta skript direkt via Lua (`lua test_add.lua`) kommer att producera något som:

```
.
Körde 1 test på 0.001 sekunder, 1 framgång, 0 misslyckanden
```

Både Busted och LuaUnit erbjuder omfattande funktioner för att hantera olika testscenarier, inklusive mockning, spionering och asynkron testning. Valet mellan dem ligger i de specifika behoven av ditt projekt och din personliga preferens gällande syntax och funktionalitet.
