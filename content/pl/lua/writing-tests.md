---
title:                "Pisanie testów"
date:                  2024-01-19
html_title:           "Bash: Pisanie testów"
simple_title:         "Pisanie testów"
programming_language: "Lua"
category:             "Lua"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/lua/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why?
Testy w programowaniu to skrypty sprawdzające poprawność działania kodu. Programiści je piszą, aby zapewnić jakość, wykryć błędy i uniknąć kłopotów w przyszłości.

## How to:
Lua nie ma biblioteki standardowej dla testów, ale można używać zewnętrznych. Oto prosty przykład użycia `busted`:

```Lua
describe("addition", function()
    it("adds two numbers", function()
        assert.are.equal(4, 2+2)
    end)
end)
```

Uruchomienie `busted` powinno dać taki wynik:
```
●●
2 successes / 0 failures / 0 errors / 0 pending : 0.0 seconds
```

## Deep Dive:
Lua nie zawsze miała narzędzia do testowania. Wcześniej programiści używali prostych asercji lub pisali własny kod testujący. Dostępne alternatywy to `busted`, `luassert`, czy `TestMore`. Implementacja testów zależy od potrzeb projektu i preferowanego stylu testowania (TDD, BDD).

## See Also:
- [Luassert - Assertion library for Lua](https://github.com/Olivine-Labs/luassert)
- [LuaUnit - xUnit for Lua](https://luaunit.readthedocs.io/en/latest/)
