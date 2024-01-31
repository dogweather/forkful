---
title:                "Skriving av tester"
date:                  2024-01-19
simple_title:         "Skriving av tester"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/lua/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why?
Tester er koder som sjekker at programmet gjør det det skal. Programmerere skriver tester for å fange feil tidlig, spare tid og sikre kvalitet.

## How to:
```Lua
local Calculator = {}

function Calculator.add(a, b)
    return a + b
end

-- Enkel test for å sjekke addisjonsfunksjonen
local function testAdd()
    local result = Calculator.add(2, 2)
    assert(result == 4, "Forventet 4, fikk " .. result)
end

testAdd()
print("testAdd passert!")
```
Kjører du koden, får du følgende output:
```
testAdd passert!
```

## Deep Dive
Tester i Lua kan skrives med innebygde funksjoner som `assert`. Historisk har Lua-miljøet utviklet testrammeverk som Busted og LuaUnit. Disse tilbyr mer funksjonalitet som testoppsett/avvikling og resultatformat. Ved å isolere kode og bruke "mocks" kan man teste individuelle deler uten avhengigheter.

## See Also
- [LuaUnit on GitHub](https://github.com/bluebird75/luaunit)
- [Programming in Lua (book)](https://www.lua.org/pil/)
