---
title:                "Scrivere un file di testo"
date:                  2024-01-19
html_title:           "Arduino: Scrivere un file di testo"
simple_title:         "Scrivere un file di testo"

category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/fish-shell/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?
Scrivere un file di testo consiste nel salvare testo su disco. Programmatori lo fanno per persistenza dati, configurazioni o logging.

## How to:
Creare un file `esempio.txt` e scrivere "Ciao, mondo!":

```Fish Shell
echo "Ciao, mondo!" > esempio.txt
```

Aggiungere una riga allo stesso file:

```Fish Shell
echo "Aggiungo una nuova riga." >> esempio.txt
```

Contenuto di `esempio.txt`:

```
Ciao, mondo!
Aggiungo una nuova riga.
```

## Deep Dive
Fish Shell, introdotto nel 2005, è noto per la sua semplicità. Confronto a Bash, Fish ha una sintassi più pulita e funzioni moderne come autosuggestion. Scrivere file è standard, ma attenzione al overwrite (>) e append (>>). 

## See Also
- Documentazione ufficiale Fish: https://fishshell.com/docs/current/index.html
- Tutorial Fish Scripting: https://fishshell.com/docs/current/tutorial.html
- Bash vs. Fish: https://www.baeldung.com/linux/bash-vs-fish
