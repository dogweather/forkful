---
title:                "Scrivere su errore standard"
html_title:           "Python: Scrivere su errore standard"
simple_title:         "Scrivere su errore standard"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/python/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Cos'è e perché?

Scrivere a standard error è il processo di inviare output ai messaggi di errore anziché al normale output del programma. I programmatori fanno questo per fornire informazioni su eventuali errori che si verificano durante l'esecuzione del codice, facilitando così il processo di debugging.

## Come fare:

Ecco un esempio di codice Python che utilizza la funzione di stampa per inviare un messaggio a standard error:

```Python
import sys
print("Errore: divisione per zero", file=sys.stderr)
```

Output:

```
Errore: divisione per zero
```

## Approfondimento:

Scrive a standard error esiste fin dai primi tempi della programmazione, quando i programmi venivano eseguiti su terminali. Quando si utilizzano molteplici output, come ad esempio standard output e standard error, si consiglia di utilizzare uno dei molti moduli disponibili per facilitare il processo.

In alternativa, si può anche utilizzare la funzione ```logging.error()``` per inviare messaggi di errore a standard error. Questo offre ulteriori funzionalità come la possibilità di specificare il livello di gravità dell'errore.

Per implementare efficientemente la scrittura a standard error, è importante assicurarsi che il codice sia compatibile con tutti i sistemi operativi su cui si desidera eseguirlo, in quanto il modo di gestire i messaggi di errore potrebbe variare tra di loro.

## Vedi anche:

- Documentazione ufficiale Python: https://docs.python.org/3/library/sys.html#sys.stderr
- Tutorial dettagliato su come utilizzare la scrittura a standard error: https://realpython.com/python-logging/
- Spiegazione più tecnica delle differenze tra standard output e standard error: https://www.jstorimer.com/blogs/workingwithcode/7766119-when-to-use-stdout-and-when-to-use-stderr