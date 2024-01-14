---
title:    "Python: Leggere un file di testo."
keywords: ["Python"]
---

{{< edit_this_page >}}

## Perché

La lettura di un file di testo è una delle attività più comuni nella programmazione Python. Potresti avere bisogno di leggere un file di dati per analizzarlo, modificarlo o utilizzarlo come input per il tuo programma. In questo articolo, imparerai come leggere un file di testo in Python e ottenere il contenuto all'interno.

## Come Fare

Per leggere un file di testo in Python, useremo la funzione `open()`. Questa funzione ci permette di aprire un file specificando il suo percorso e una modalità di lettura (ad esempio "r" per lettura). Una volta aperto il file, possiamo utilizzare il metodo `readlines()` per ottenere una lista di tutte le linee nel file.

```
```Python
f = open("mio_file.txt", "r")
linee = f.readlines()
f.close()

print(linee)
```

Questo codice aprirà il file "mio_file.txt" e stamperà tutte le sue linee. Assicurati di utilizzare il metodo `close()` per chiudere il file dopo che hai finito di leggerlo.

## Deep Dive

La funzione `open()` ha molti altri parametri opzionali che ti permettono di specificare il formato del file, la modalità di scrittura e altro ancora. Inoltre, il metodo `readlines()` restituisce una lista di stringhe, quindi potresti dover manipolare i dati e convertirli in un formato più adatto alle tue esigenze. Inoltre, è importante gestire gli errori durante la lettura del file in modo da evitare crash del programma o perdita di dati.

## Vedi Anche

- [Documentazione ufficiale di Python su open()](https://docs.python.org/3/library/functions.html#open)
- [Tutorial su file e I/O in Python](https://www.programiz.com/python-programming/file-operation)
- [Altro tutorial su lettura e scrittura di file in Python](https://realpython.com/read-write-files-python/)