---
title:                "Scrivere su standard di errore"
html_title:           "Python: Scrivere su standard di errore"
simple_title:         "Scrivere su standard di errore"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/python/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Perché

Scrivere su standard error è utile quando si vuole visualizzare informazioni di errore o di debug nel terminale invece di mostrarle direttamente nella propria applicazione.

## Come fare

Per scrivere su standard error in Python, è necessario utilizzare il modulo `sys` e il suo metodo `stderr.write()`. Ecco un esempio:

```Python
import sys

sys.stderr.write("Errore: nome file non trovato\n")
```

Questo codice scriverà la stringa "Errore: nome file non trovato" su standard error, invece che su standard output. 

```
Errore: nome file non trovato
```

## Approfondimento

La differenza tra standard error e standard output può sembrare sottile, ma è importante capire quando utilizzare uno o l'altro. In generale, standard error dovrebbe essere utilizzato per mostrare informazioni di errore o di debug, mentre standard output dovrebbe essere utilizzato per tutte le altre informazioni.

Inoltre, è possibile redirezionare standard error al di fuori del terminale, ad esempio in un file di log, utilizzando il simbolo `>` o il comando `tee`.

## Vedi anche

- Documentazione ufficiale del modulo `sys`: https://docs.python.org/3/library/sys.html
- Tutorial su come redirezionare output e errori in Linux: https://www.howtogeek.com/435903/how-to-use-the-linux-tee-command/