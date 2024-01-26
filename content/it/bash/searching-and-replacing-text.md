---
title:                "Ricerca e sostituzione del testo"
date:                  2024-01-20T17:57:07.531157-07:00
model:                 gpt-4-1106-preview
simple_title:         "Ricerca e sostituzione del testo"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/bash/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## What & Why?
La ricerca e la sostituzione di testo permettono di trovare stringhe specifiche e modificarle velocemente. I programmatori lo fanno per correggere errori, aggiornare codici o dati, e automatizzare l'editing di file numerosi.

## How to:
Ecco alcuni comandi Bash basilari per cercare e sostituire testo.

Usa `grep` per cercare:
```Bash
grep "cerca" file.txt
```

Sostituisci testo con `sed`:
```Bash
sed -i 's/vecchio/nuovo/g' file.txt
```

Esempio con output:
```Bash
echo "Ciao mondo" > file.txt
sed -i 's/mondo/mondi/g' file.txt
cat file.txt
```
Output:
```
Ciao mondi
```

## Deep Dive
Stringere la cinghia e addentrarsi: la capacità di cercare e sostituire è antica quanto i primi editor di testo. Su Unix, `sed` (stream editor) è lo strumento classico, usato dagli anni '70. Alternativa moderna, `awk` gestisce dati complessi, e `perl`, con espressioni regolari potenti, per elaborazioni avanzate.

Dettagli implementativi: `sed` lavora su flussi di input modificandoli al volo. Usa espressioni regolari (regex), che permettono ricerche flessibili e potenti.

## See Also
Rimani aggiornato e approfondisci:
- [GNU sed manual](https://www.gnu.org/software/sed/manual/)
- [Grep manual](https://www.gnu.org/software/grep/manual/grep.html)
- [AWK A Tutorial and Introduction](https://www.grymoire.com/Unix/Awk.html)
- [Perl regex documentation](https://perldoc.perl.org/perlre)
