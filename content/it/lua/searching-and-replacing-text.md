---
title:                "Ricerca e sostituzione del testo"
html_title:           "Arduino: Ricerca e sostituzione del testo"
simple_title:         "Ricerca e sostituzione del testo"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/lua/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Che cos'è & Perché?
La ricerca e sostituzione di testo è il processo di trovare e modificare specifici segmenti di testo all'interno di una stringa. Lo usiamo quanto vogliamo modificare dati testuali in modo sistematico e rapido.

## Come fare:
Ecco come puoi ricercare e sostituire testo in Lua. Utilizzeremo la funzione gsub() integrata nel linguaggio.

```Lua
s = "Ciao, mondo!"
s = string.gsub(s, "mondo", "universo")
print(s)  -- output: Ciao, universo!
```

Nell'esempio precedente, il codice cerca la parola "mondo" nella stringa s e la sostituisce con "universo".

## Approfondimenti
La funzione gsub() in Lua è una delle diverse funzioni di manipolazione delle stringhe messe a disposizione fin dalle primissime versioni del linguaggio. Nel corso degli anni, Lua ha cambiato significativamente il modo in cui si manipolano le stringhe, ma il concetto di ricerca e sostituzione di testo è rimasto ovviamente costante.

In termini di alternative, Lua offre anche la funzione find(), che può essere usata per trovare la posizione di un testo specifico all'interno di una stringa. Ma note che find() non sostituisce il testo, quindi dovresti utilizzarla insieme a altre funzioni per fare questo.

La funzione gsub() sfrutta i pattern dell'espressione regolare per inquadrare la logica di ricerca e sostituzione. Questo significa che puoi avere pattern complicati e sostituzioni dinamiche.

## Vedi anche
Per ulteriori informazioni sulle stringhe e sugli altri costrutti di programmazione Lua, dai un'occhiata ai seguenti collegamenti:

1. Lua 5.3 Reference Manual - String Manipulation: https://www.lua.org/manual/5.3/manual.html#6.4
2. Programming in Lua: String Library: https://www.lua.org/pil/20.html
3. Tutorial su Lua - Stringhe: https://www.tutorialspoint.com/lua/lua_strings.htm