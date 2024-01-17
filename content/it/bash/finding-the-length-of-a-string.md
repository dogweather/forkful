---
title:                "Trova la lunghezza di una stringa"
html_title:           "Bash: Trova la lunghezza di una stringa"
simple_title:         "Trova la lunghezza di una stringa"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/bash/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Che cos'è e perché?

Trovare la lunghezza di una stringa è un'operazione fondamentale nella programmazione Bash. Essenzialmente, si tratta di determinare il numero di caratteri all'interno di una stringa. Questo è importante principalmente per la gestione dei dati e delle stringhe all'interno di uno script.

## Come fare:

Per ottenere la lunghezza di una stringa in Bash, è possibile utilizzare il comando integrato "expr" seguito da "length". Ecco un esempio di codice con output:

```Bash
stringa="Ciao mondo!"
lunghezza=$(expr length "$stringa")
echo "La lunghezza della stringa è $lunghezza"
```
**Output:** La lunghezza della stringa è 11

## Approfondimenti:

Prima del comando "expr", gli sviluppatori di Bash usufruivano del comando "strlen" per ottenere la lunghezza di una stringa. Questo comando era disponibile solo nelle versioni precedenti di Bash e attualmente è deprecato.

Inoltre, esistono anche altre metodologie per ottenere la lunghezza di una stringa in Bash, come ad esempio utilizzando la funzione "wc" o "awk". Tuttavia, il comando "expr" risulta il più utilizzato e semplice da implementare per raggiungere lo stesso obiettivo.

## Vedi anche:

- [BashGuide/ Parameter Expansion](http://mywiki.wooledge.org/BashGuide/Parameters)
- [Bash - Parameter Substitution](https://www.tldp.org/LDP/abs/html/parameter-substitution.html)