---
title:                "Eliminazione di caratteri corrispondenti a un pattern"
html_title:           "Bash: Eliminazione di caratteri corrispondenti a un pattern"
simple_title:         "Eliminazione di caratteri corrispondenti a un pattern"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/bash/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Perché

Ti sei mai trovato in una situazione in cui avevi bisogno di eliminare dei caratteri che corrispondevano a uno specifico pattern nel tuo codice Bash? Questo articolo ti mostrerà come farlo in modo semplice ed efficiente.

## Come

```Bash
# Elimina tutti i caratteri "a" dalla stringa
echo "casa" | sed 's/a//g'
# Output: cs

# Elimina tutti i caratteri numerici dalla stringa
echo "abc123" | sed 's/[0-9]//g'
# Output: abc
```

## Deep Dive

La soluzione che abbiamo visto sopra utilizza l'utility "sed" che viene utilizzata per modificare il testo. Nel codice, abbiamo specificato il parametro "s" che indica la sostituzione e il pattern che vogliamo eliminare tra le virgolette dopo la barra. Utilizzando "g" alla fine, garantiamo che la sostituzione venga fatta su tutte le occorrenze del pattern nella stringa.

Inoltre, esiste anche un'opzione "-i" che permette di modificare direttamente il file senza dover visualizzare l'output sul terminale.

## Vedi anche

- [Manuale Bash](https://www.gnu.org/software/bash/manual/)
- [Documentazione di sed](https://www.gnu.org/software/sed/manual/)

Se vuoi saperne di più su come manipolare il testo in Bash, ti consiglio di dare un'occhiata a questi link per avere una conoscenza più approfondita. Buon hacking!