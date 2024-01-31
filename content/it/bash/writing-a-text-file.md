---
title:                "Scrivere un file di testo"
date:                  2024-01-19
html_title:           "Arduino: Scrivere un file di testo"
simple_title:         "Scrivere un file di testo"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/bash/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?
Scrivere un file di testo permette di salvare dati in modo persistente. Programmatori lo fanno per memorizzare configurazioni, documentazioni e script.

## How to:
### Creare un file di testo:
```Bash
echo "Ciao, mondo!" > file.txt
```
### Aggiungere al file esistente:
```Bash
echo "Salve, universo!" >> file.txt
```
### Visualizzare il contenuto del file:
```Bash
cat file.txt
```
**Output:**
```
Ciao, mondo!
Salve, universo!
```

## Deep Dive
Nella programmazione Unix, il reindirizzamento in file è uno standard dagli anni '70. Alternatives include `printf` o editor come `vim`. Nei dettagli, `>` sovrascrive il file mentre `>>` aggiunge al file senza cancellare il contenuto esistente.

## See Also
- [GNU Bash Manual](https://www.gnu.org/software/bash/manual/)
- [Advanced Bash-Scripting Guide](http://www.tldp.org/LDP/abs/html/)
- [Stack Overflow: Bash](https://stackoverflow.com/questions/tagged/bash)
