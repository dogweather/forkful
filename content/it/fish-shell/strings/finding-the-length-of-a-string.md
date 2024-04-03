---
date: 2024-01-20 17:47:26.800581-07:00
description: "In Fish Shell, troviamo la lunghezza di una stringa per sapere quanti\
  \ caratteri contiene. Quest'operazione \xE8 utile nella manipolazione dei testi\
  \ e nella\u2026"
lastmod: '2024-03-13T22:44:43.847119-06:00'
model: gpt-4-1106-preview
summary: In Fish Shell, troviamo la lunghezza di una stringa per sapere quanti caratteri
  contiene.
title: Trovare la lunghezza di una stringa
weight: 7
---

## How to:
Per calcolare la lunghezza di una stringa in Fish, utilizziamo il comando `string length`. Ecco un esempio semplice:

```Fish Shell
set mia_stringa "Ciao, mondo!"
echo (string length "$mia_stringa")
```

Output:

```
12
```

## Deep Dive
La funzionalità di calcolo della lunghezza di una stringa esiste in molti linguaggi di programmazione e shell. In Fish Shell, il comando `string` è stato introdotto nelle versioni più recenti, sostituendo l'uso di comandi esterni come `expr` o `wc -m`.

Alternativamente, potresti usare `wc -m`, ma `string length` è preferibile perché è una funzione integrata di Fish, quindi più veloce e facile da usare in script.

Dettagli dell'implementazione:
- `string length` conta i caratteri Unicode correttamente, anche con emoji e altri caratteri multibyte.
- Funziona direttamente su variabili e non richiede piping o subshell, rendendo lo script più leggibile e efficiente.

## See Also
- Documentazione ufficiale di Fish sul comando `string`: https://fishshell.com/docs/current/cmds/string.html
- Panoramica sui metodi di manipolazione delle stringhe in vari linguaggi: https://en.wikibooks.org/wiki/Algorithm_Implementation/Strings/String_length
