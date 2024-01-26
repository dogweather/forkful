---
title:                "Scrivere sull'errore standard"
html_title:           "Arduino: Scrivere sull'errore standard"
simple_title:         "Scrivere sull'errore standard"
programming_language: "Lua"
category:             "Lua"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/lua/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why?
Scrivere su standard error permette di separare l'output normale da quello degli errori. I programmatori lo fanno per facilitare il debugging e la gestione degli errori.

## How to:
In Lua, `io.stderr:write()` scrive su standard error.

```Lua
-- Invio di un messaggio di errore a stderr
io.stderr:write("Errore: non è possibile elaborare il file.\n")

-- Visualizzando l'output usando os.execute() per simulare il redirezionamento
os.execute("echo 'Messaggio normale'")  -- Output su stdout
os.execute("lua script.lua 1>&2")  -- Redirezione di stdout a stderr
```
Output di esempio:
```
Messaggio normale
Errore: non è possibile elaborare il file.
```

## Deep Dive
Lua non fa distinzioni tra stdout e stderr nativamente come altri linguaggi; `io.stderr` è uno stream separato fornito per convenzione. Alternativamente, puoi usare `print` o `io.write` per stdout e redirigere da terminale. Da un punto di vista storico, la distinzione tra stdout e stderr risale ai primi sistemi Unix, permettendo agli utenti di differenziare tra output ordinario e messaggi di errore.

## See Also
- Lua 5.4 Reference Manual: https://www.lua.org/manual/5.4/
- Programming in Lua (4th edition): https://www.lua.org/pil/contents.html
- Unix Standard Streams - Wikipedia: https://en.wikipedia.org/wiki/Standard_streams
