---
title:                "Fish Shell: Ricerca della lunghezza di una stringa"
programming_language: "Fish Shell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/fish-shell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

##Perché

Trovare la lunghezza di una stringa è un'operazione molto comune nella programmazione. È utile per molteplici scopi, come ad esempio manipolare gli input dell'utente o controllare la validità di una password.

## Come fare

Per ottenere la lunghezza di una stringa utilizzando Fish Shell, è possibile utilizzare il comando `string length`. Ad esempio:

```
Fish Shell# string length "Ciao a tutti!"
15
```

Nell'esempio sopra, il comando `string length` calcola la lunghezza della stringa "Ciao a tutti!", che è di 15 caratteri. 

## Approfondimento

Il comando `string length` conta anche gli spazi vuoti all'interno della stringa. Inoltre, è possibile utilizzare il wildcard `*` per ottenere la lunghezza di una variabile contenente una stringa. Ad esempio:

```
Fish Shell# set greeting "Buongiorno!"
Fish Shell# string length $greeting
11
```

In questo caso, la variabile `greeting` è stata utilizzata come input per il comando `string length`, che ne ha restituito la lunghezza.

Il comando `string length` può essere combinato con altri comandi Fish per ottenere risultati più complessi. Ad esempio, è possibile utilizzare `string length` insieme a `string sub` per ottenere la lunghezza di una sottostringa. 

## Vedi anche

- Documentazione ufficiale di Fish Shell su `string length`: https://fishshell.com/docs/current/cmds-string.html#string-length
- Guida completa su modifica di stringhe in Fish Shell: https://fishshell.com/docs/current/tutorial.html#tut_strings 
- Esempi pratici di utilizzo di `string length` in Fish Shell: https://scotch.io/tutorials/manipulate-strings-with-terminal-commands-in-fish-shell#toc-string-length