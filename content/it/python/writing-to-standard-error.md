---
title:                "Python: Scrivere su standard error"
simple_title:         "Scrivere su standard error"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/python/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Perché

Scrivere a standard error (stderr) è un'importante abilità per ogni programmatore Python. Quando un programma viene eseguito, gli errori vengono spesso inviati a stderr anziché a stdout (standard output). In questo modo, è possibile distinguere facilmente gli errori dai risultati del programma e risolverli in modo efficace.

## Come

Per scrivere a stderr in Python, è necessario importare il modulo "sys" e usare il metodo "stderr" per scrivere il messaggio desiderato.

```
import sys

sys.stderr.write("Questo è un messaggio di errore!")
```

L'esempio sopra utilizza il metodo "write" su "sys.stderr" per scrivere il messaggio tra parentesi direttamente a stderr. Assicurati di includere un carattere di ritorno a capo ("/n") alla fine del messaggio per rendere la formattazione corretta.

L'output di questo programma sarà:

```
Questo è un messaggio di errore!
```

## Approfondimento

Ora che sai come scrivere a stderr, è importante capire quando e perché dovresti farlo. Il principale motivo per cui si dovrebbe scrivere a stderr è distinguere gli errori dai risultati del programma. Inoltre, questo metodo è utile per registrare informazioni di debug durante lo sviluppo o per avvisare l'utente di eventuali problemi critici.

Un'altra cosa importante da notare è che stderr viene solitamente trasmesso al terminale e non viene salvato nei file di log. Ciò significa che se si vuole registrare un errore o un messaggio di debug in un file, è necessario utilizzare il metodo "write" su un file aperto anziché su "sys.stderr".

## Vedi anche

- Documentazione ufficiale di Python su sys.stderr: https://docs.python.org/3/library/sys.html#sys.stderr
- Tutorial su come scrivere e gestire errori in Python: https://www.digitalocean.com/community/tutorials/how-to- write-and-handle-errors-in-python-3
- Spiegazione dettagliata su quando usare stdout e stderr in Python: https://realpython.com/python-print/