---
title:                "Maiuscolizzare una stringa"
date:                  2024-01-19
html_title:           "Bash: Maiuscolizzare una stringa"
simple_title:         "Maiuscolizzare una stringa"

category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/fish-shell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Capitalizzare una stringa vuol dire trasformare tutte le lettere in maiuscole. I programmatori lo fanno per uniformità, confronti di testi o per estetica.

## How to:
Fish non ha un comando integrato per capitalizzare tutto, ma possiamo usare 'string' e 'awk' così:

```Fish Shell
echo "cia mondo" | string upper # usa 'string upper' per capitalizzare
```

Output:
```
CIA MONDO
```

Oppure con awk:

```Fish Shell
echo "cia mondo" | awk '{ print toupper($0) }' # awk per capitalizzare
```

Output:
```
CIA MONDO
```

## Deep Dive
Prima dell'introduzione di `string`, capitava di dover installare strumenti esterni o scrivere script complicati. Con `string`, da Fish 2.3.0, la manipolazione delle stringhe è diventata semplice. Tuttavia, alcuni preferiscono usare `awk`, `tr` o `sed` per abitudine o per compatibilità con altri shell.

`awk` è potente per il processamento di testi e file. `tr` è un comando UNIX più vecchio che trasforma o elimina caratteri. `sed` è per l'editing di flussi di testi.

Ognuno ha i suoi casi d'uso, ma in Fish, `string` è spesso il modo più diretto e pulito.

## See Also
- Documentazione di Fish su `string`: https://fishshell.com/docs/current/cmds/string.html
- AWK User’s Guide: https://www.gnu.org/software/gawk/manual/gawk.html
- Unix `tr` command: https://man7.org/linux/man-pages/man1/tr.1.html
- GNU `sed`: https://www.gnu.org/software/sed/manual/sed.html
