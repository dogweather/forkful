---
title:                "Scrivere su standard error"
html_title:           "Bash: Scrivere su standard error"
simple_title:         "Scrivere su standard error"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/bash/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Perché

Scrivere su standard error è un modo per visualizzare messaggi di errore e diagnostica per il codice Bash che stai eseguendo. Ciò è particolarmente utile quando si desidera tenere traccia di eventuali problemi o bug durante l'esecuzione di script.

## Come fare

Per scrivere su standard error in Bash, puoi utilizzare il comando `>&2` o `echo "test" 1>&2` all'interno delle tue istruzioni. Ecco un esempio di codice Bash che utilizza `>&2`:

```Bash
#!/bin/bash

# Questo codice scrive "C'è stato un errore" su standard error
# se l'utente non specifica il file da copiare

if [ -z "$1" ]; then
  echo "Usage: $0 <file_origin> <file_destination>" >&2
  exit 1
fi
```

Esempio di output:

```Bash
$ ./copiacartella.sh

Usage: ./copiacartella.sh <file_origin> <file_destination>
```

## Approfondimento

Il simbolo `>&2` viene utilizzato per redirigere l'output di un comando su standard error invece che su standard output (di default). In questo modo, tutti i messaggi di errore generati dal comando vengono visualizzati in rosso, rendendoli facilmente distinguibili dai messaggi normali. Ciò può essere utile quando si eseguono script più complessi e si desidera tenere traccia di eventuali problemi.

Un altro modo per scrivere su standard error è utilizzare il comando `echo` con `1>&2`. Per esempio: `echo "test" 1>&2`. In questo caso, il parametro `1` indica che l'output deve andare su standard output (il cui numero è 1) e il simbolo `>&` indica la redirezione su standard error (il cui numero è 2).

## Vedi anche

- [Guida all'uso di Bash](https://www.gnu.org/software/bash/manual/html_node/index.html)
- [Documentazione di Bash](https://www.gnu.org/software/bash/documentation/)