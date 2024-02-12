---
title:                "Scrivere sull'errore standard"
aliases:
- /it/lua/writing-to-standard-error/
date:                  2024-02-03T19:33:50.941189-07:00
model:                 gpt-4-0125-preview
simple_title:         "Scrivere sull'errore standard"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/lua/writing-to-standard-error.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cosa e Perché?
Scrivere su standard error (stderr) consiste nel dirigere messaggi di errore e output diagnostici su un canale separato, distinto dallo standard output (stdout). I programmatori fanno ciò per differenziare i risultati regolari del programma dalle informazioni di errore, razionalizzando i processi di debug e registrazione.

## Come fare:
In Lua, scrivere su stderr può essere realizzato utilizzando la funzione `io.stderr:write()`. Ecco come è possibile scrivere un semplice messaggio di errore su stderr:

```lua
io.stderr:write("Errore: Input non valido.\n")
```

Se è necessario produrre in output una variabile o combinare più pezzi di dati, concatenarli all'interno della funzione write:

```lua
local errorMessage = "Input non valido."
io.stderr:write("Errore: " .. errorMessage .. "\n")
```

**Esempio di Output su stderr:**
```
Errore: Input non valido.
```

Per scenari più complessi, o quando si lavora con applicazioni di maggiori dimensioni, potreste considerare l'uso di librerie di logging di terze parti come LuaLogging. Con LuaLogging, è possibile indirizzare i log verso destinazioni differenti, inclusa stderr. Ecco un breve esempio:

Prima, assicurarsi che LuaLogging sia installato usando LuaRocks:

```
luarocks install lualogging
```

Poi, per scrivere un messaggio di errore su stderr usando LuaLogging:

```lua
local logging = require("logging")
local logger = logging.stderr()
logger:error("Errore: Input non valido.")
```

Questo approccio offre il vantaggio di una registrazione standardizzata in tutta l'applicazione, con la flessibilità aggiuntiva di impostare livelli di log (ad es., ERROR, WARN, INFO) tramite un'API semplice.
