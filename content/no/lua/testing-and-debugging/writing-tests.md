---
title:                "Skrive tester"
aliases: - /no/lua/writing-tests.md
date:                  2024-02-03T19:31:26.559625-07:00
model:                 gpt-4-0125-preview
simple_title:         "Skrive tester"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/lua/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å skrive tester i programmering innebærer å lage små, separate deler av kode for automatisk å verifisere at ulike deler av applikasjonen din fungerer som forventet. For Lua-programmerere sikrer testing pålitelighet og hjelper til med å opprettholde kodekvalitet, det fremskynder feilsøkingsprosessen og gjør endringer i kodebasen sikrere.

## Hvordan:

Lua, som er et lettvekts, men kraftfullt skriptspråk, inkluderer ikke et innebygd testrammeverk. Imidlertid gjør tredjepartsbiblioteker som Busted og LuaUnit testingen relativt grei. Her vil vi se på eksempler ved bruk av begge.

### Bruker Busted

Busted er et populært Lua-testrammeverk som tilbyr en fleksibel måte å skrive tester på. Først installerer du Busted gjennom LuaRocks (Luas pakkebehandler) med `luarocks install busted`. Når installert, kan du skrive testene dine. Her er en enkel test for en funksjon `add` som summerer to tall:

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

describe("Add funksjon", function()
  it("skal legge sammen to tall korrekt", function()
    assert.are.equal(5, add(2, 3))
  end)
end)
```

For å kjøre testene, eksekverer du `busted` i terminalen din. Eksempelutdata for en vellykket test ville se slik ut:

```
●
1 suksess / 0 feil / 0 feil / 0 ventende : 0.002 sekunder
```

### Bruker LuaUnit

LuaUnit er et annet testrammeverk som følger xUnit-konvensjoner og er enkelt å sette opp. Installer LuaUnit via LuaRocks ved å bruke `luarocks install luaunit`. Her er hvordan du kan skrive en lignende test som ovenfor med LuaUnit:

```lua
-- add.lua forblir den samme

-- test_add.lua
luaunit = require('luaunit')
local add = require('add')

function testAdd()
  luaunit.assertEquals(add(2, 3), 5)
end

os.exit(luaunit.LuaUnit.run())
```

Å kjøre dette skriptet direkte via Lua (`lua test_add.lua`) vil gi noe slikt som utdata:

```
.
Kjørte 1 tester på 0.001 sekunder, 1 suksess, 0 feil
```

Både Busted og LuaUnit tilbyr omfattende funksjoner for å håndtere ulike testscenarier, inkludert mocking, spionering, og asynkron testing. Valget mellom dem ligger i de spesifikke behovene til prosjektet ditt og din personlige preferanse når det gjelder syntaks og funksjonalitet.
