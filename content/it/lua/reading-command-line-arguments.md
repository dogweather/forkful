---
title:                "Lettura degli argomenti della riga di comando"
html_title:           "Java: Lettura degli argomenti della riga di comando"
simple_title:         "Lettura degli argomenti della riga di comando"
programming_language: "Lua"
category:             "Lua"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/lua/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Che Cosa e Perché?

Leggere gli argomenti da riga di comando in Lua significa interpretare i dati che l'utente inserisce quando esegue un programma da terminale. Gli sviluppatori lo fanno perché consente una maggiore flessibilità e interattività del programma.

# Come Fare:

Per leggere gli argomenti da riga di comando in Lua, usiamo la tabella globale ```arg```. Ecco un esempio di codice:

```Lua
-- Stampa il nome dell'eseguibile
print("Nome dell'eseguibile: " .. arg[0])

-- Stampa i parametri passati
for i = 1, #arg do
  print("Parametro " .. i .. ": " .. arg[i])
end
```

Se eseguiamo il programma con `./miaApp par1 par2`, vedremo l'output:

```Lua
Nome dell'eseguibile: ./miaApp
Parametro 1: par1
Parametro 2: par2
```

# Approfondimento

(1) Contesto Storico: La tabella globale `arg` è stata introdotta in Lua 5.1 per semplificare la lettura di argomenti da riga di comando, un concetto molto comune nella programmazione shell Unix.

(2) Alternative: Prima di Lua 5.1, si utilizzava la funzione `getopt` per analizzare gli argomenti. Tuttavia, questa funzione è molto meno intuitiva e user-friendly.

(3) Dettagli di Implementazione: La tabella `arg` non solo conserva gli argomenti passati, ma ha anche indici negativi che puntano ai nomi dei file di script e degli eseguibili del programma.

# Guida alla Consultazione:

- Documentazione ufficiale Lua: [https://www.lua.org/manual/5.4/manual.html#pdf-arg](https://www.lua.org/manual/5.4/manual.html#pdf-arg)

- The Programming in Lua online book: [https://www.lua.org/pil/contents.html](https://www.lua.org/pil/contents.html)

- Imparare Lua dal tutorial di Derek Banas: [http://www.newthinktank.com/2015/11/learn-lua-one-video/](http://www.newthinktank.com/2015/11/learn-lua-one-video/)