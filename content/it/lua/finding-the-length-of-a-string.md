---
title:                "Trovare la lunghezza di una stringa"
html_title:           "Lua: Trovare la lunghezza di una stringa"
simple_title:         "Trovare la lunghezza di una stringa"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/lua/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Cos'è e perché?

Trovare la lunghezza di una stringa è un'operazione comune nella programmazione. Si tratta di determinare il numero di caratteri presenti in una stringa di testo. Questo è utile per molte ragioni, come ad esempio la validazione dei dati inseriti dall'utente o la gestione dei limiti di lunghezza per un determinato campo.

## Come fare:

```Lua
-- Definiamo una stringa
stringa = "Ciao amici!"

-- Utilizziamo la funzione # per trovare la lunghezza
lunghezza = #stringa

-- Stampa della lunghezza
print(lunghezza)

-- Output: 11
```

## Approfondimenti:

La funzione # per trovare la lunghezza delle stringhe è stata introdotta nella versione 5.1 di Lua. In precedenza, gli sviluppatori dovevano utilizzare la libreria string per eseguire questa operazione. Alcune alternative alla funzione # includono l'utilizzo della funzione len della libreria string e la creazione di un ciclo per contare i caratteri di una stringa.

## Vedi anche:

- [Documentazione Lua sulla funzione #](https://www.lua.org/manual/5.1/manual.html#2.5.5)
- [Tutorial su come utilizzare la libreria string in Lua](https://www.tutorialspoint.com/lua/lua_string_library.htm)
- [Discussione sul forum di Lua su alternative alla funzione #](https://www.lua.org/pipermail/lua-l/2008-June/thread.html#55129)