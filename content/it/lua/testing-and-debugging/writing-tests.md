---
aliases:
- /it/lua/writing-tests/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:31:18.950037-07:00
description: "Scrivere test nella programmazione implica la creazione di piccoli pezzi\
  \ di codice separati per verificare automaticamente che diverse parti della tua\u2026"
lastmod: 2024-02-18 23:08:56.014058
model: gpt-4-0125-preview
summary: "Scrivere test nella programmazione implica la creazione di piccoli pezzi\
  \ di codice separati per verificare automaticamente che diverse parti della tua\u2026"
title: Scrivere test
---

{{< edit_this_page >}}

## Cosa e perché?

Scrivere test nella programmazione implica la creazione di piccoli pezzi di codice separati per verificare automaticamente che diverse parti della tua applicazione funzionino come previsto. Per i programmatori Lua, effettuare test garantisce affidabilità e aiuta a mantenere la qualità del codice, velocizzando il processo di debug e rendendo le modifiche al codice base più sicure.

## Come fare:

Lua, essendo un linguaggio di scripting leggero eppure potente, non include un framework di test incorporato. Tuttavia, librerie di terze parti come Busted e LuaUnit rendono il testing relativamente semplice. Qui, vedremo esempi utilizzando entrambi.

### Utilizzando Busted

Busted è un popolare framework di test per Lua che offre un modo flessibile per scrivere test. Innanzitutto, installa Busted tramite LuaRocks (il gestore di pacchetti di Lua) con `luarocks install busted`. Una volta installato, puoi scrivere i tuoi test. Ecco un semplice test per una funzione `add` che somma due numeri:

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

describe("Funzione di addizione", function()
  it("dovrebbe sommare due numeri correttamente", function()
    assert.are.equal(5, add(2, 3))
  end)
end)
```

Per eseguire i test, esegui `busted` nel tuo terminale. L'output di esempio per un test superato sarebbe:

```
●
1 successo / 0 fallimenti / 0 errori / 0 in sospeso : 0.002 secondi
```

### Utilizzando LuaUnit

LuaUnit è un altro framework di test che segue le convenzioni di xUnit ed è facile da configurare. Installa LuaUnit tramite LuaRocks usando `luarocks install luaunit`. Ecco come potresti scrivere un test simile a quello sopra con LuaUnit:

```lua
-- add.lua rimane lo stesso

-- test_add.lua
luaunit = require('luaunit')
local add = require('add')

function testAdd()
  luaunit.assertEquals(add(2, 3), 5)
end

os.exit(luaunit.LuaUnit.run())
```

Eseguendo direttamente questo script tramite Lua (`lua test_add.lua`) produrrà un output come:

```
.
Eseguiti 1 test in 0.001 secondi, 1 successo, 0 fallimenti
```

Sia Busted che LuaUnit offrono ampie funzionalità per gestire vari scenari di test, inclusi il mocking, lo spying e il testing asincrono. La scelta tra di loro dipende dalle esigenze specifiche del tuo progetto e dalla tua preferenza personale riguardo alla sintassi e alla funzionalità.
