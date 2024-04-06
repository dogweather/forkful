---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:13:20.414469-07:00
description: "Hoe te: Historisch gezien had Lua geen ingebouwd testframework, wat\
  \ ertoe leidde dat ontwikkelaars hun eigen maakten of die van derden gebruikten,\
  \ zoals\u2026"
lastmod: '2024-04-05T21:53:50.963596-06:00'
model: gpt-4-0125-preview
summary: Historisch gezien had Lua geen ingebouwd testframework, wat ertoe leidde
  dat ontwikkelaars hun eigen maakten of die van derden gebruikten, zoals LuaUnit
  of busted.
title: Tests Schrijven
weight: 36
---

## Hoe te:
```Lua
-- Eenvoudig Lua testvoorbeeld met assert

function add(a, b)
  return a + b
end

-- Testfunctie
function testAdd()
  assert(add(2, 2) == 4)
  assert(add(-1, 1) == 0)
  print("Geslaagd voor alle add() tests.")
end

testAdd()  -- Het uitvoeren van de testfunctie
```

Uitvoer:
```
Geslaagd voor alle add() tests.
```

## Diepgaande duik
Historisch gezien had Lua geen ingebouwd testframework, wat ertoe leidde dat ontwikkelaars hun eigen maakten of die van derden gebruikten, zoals LuaUnit of busted. Met een minimalistische kern, behandelen deze frameworks de setup/afbreek, beweringen, en rapportageformaten. Alternatieven omvatten het gebruik van de native `assert` functie voor eenvoudige tests of het integreren van Lua met continuous integration (CI) systemen voor geautomatiseerd testen in verschillende omgevingen. Implementatiedetails omvatten het schrijven van testbare code, het begrijpen van het belang van testdekking en het ontwerpen van tests die zowel uitgebreid als leesbaar zijn.

## Zie ook
- LuaUnit GitHub: https://github.com/bluebird75/luaunit
- busted GitHub: https://github.com/Olivine-Labs/busted
- "Programmeren in Lua" (testhoofdstuk): https://www.lua.org/pil/11.html
- Lua Gebruikers Wiki over UnitTesting: http://lua-users.org/wiki/UnitTesting
