---
title:                "Trovare la lunghezza di una stringa"
aliases:
- /it/bash/finding-the-length-of-a-string.md
date:                  2024-01-20T17:46:40.291775-07:00
model:                 gpt-4-1106-preview
simple_title:         "Trovare la lunghezza di una stringa"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/bash/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Sapere la lunghezza di una stringa significa contarne i caratteri. Lo facciamo per verificare input dell'utente, per manipolare testo o semplicemente per controlli di validazione.

## How to:
Per trovare la lunghezza di una stringa in Bash, usiamo `${#string}`. Guarda:

```Bash
stringa="Ciao, mondo!"
echo ${#stringa}  # Output: 12
```

Oppure con una variabile:

```Bash
nome="Francesco"
lunghezza=${#nome}
echo $lunghezza  # Output: 9
```

## Deep Dive:
La sintassi `${#string}` esiste in Bash dal rilascio di Bash 2.0 nel 1996. È diretta e veloce perché incorporata nel linguaggio, a differenza di altri metodi come `expr length "$string"` o `echo "$string" | wc -m`, che invocano sottoprogrammi esterni e possono essere più lenti.

Altre opzioni includono l'uso di `awk` o `grep`, ma per la maggior parte dei casi questi sono overkill. Quando si contano stringhe contenenti caratteri multibyte come emoji o lettere accentate, la lunghezza può essere ambigua se si conta il numero di caratteri o il numero di byte. Bash di default conta i byte, il che potrebbe non essere quello che ci si aspetta con codifica multibyte come UTF-8.

## See Also:
- Bash manual (man bash): `man bash` e cerca "Parameter Expansion".
- GNU documentation on Bash (official manual): https://www.gnu.org/software/bash/manual/
- Advanced Bash-Scripting Guide: http://www.tldp.org/LDP/abs/html/
