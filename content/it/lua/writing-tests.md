---
title:                "Scrivere test"
html_title:           "Arduino: Scrivere test"
simple_title:         "Scrivere test"
programming_language: "Lua"
category:             "Lua"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/lua/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why?
Scrivere test significa creare script che automaticamente verificano se parti del tuo codice funzionano come previsto. I programmatori li usano per assicurarsi che il codice sia affidabile e per prevenire regressioni durante l'aggiornamento del software.

## How to:
In Lua, puoi scrivere test usando delle asserzioni. Un'asserzione verifica che una condizione sia vera. Se non lo è, solleva un errore. Qui sotto un esempio semplice con output.

```Lua
-- file: simple_test.lua
function add(a, b)
  return a + b
end

local test_result = add(5, 3)
assert(test_result == 8, "Expected 8, got " .. test_result)
print("Test passed! Risultato di add(5, 3) e' " .. test_result)
```

Esegui questo test e vedrai:

```
Test passed! Risultato di add(5, 3) e' 8
```

## Deep Dive
I test automatici in Lua possono essere implementati con diversi framework, come LuaUnit o busted, che seguono il concetto di xUnit presente in altri linguaggi. Sono in uso da anni come parte delle pratiche di sviluppo del software e aiutano a mantenere la qualità del codice nel tempo. In alternativa ai framework, puoi usare semplici script con asserzioni per piccoli progetti.

## See Also
- LuaUnit (https://github.com/bluebird75/luaunit)
- busted (http://olivinelabs.com/busted/)
- Articolo su Test-Driven Development (https://it.wikipedia.org/wiki/Test_driven_development)