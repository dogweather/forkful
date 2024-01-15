---
title:                "Leggere un file di testo"
html_title:           "Python: Leggere un file di testo"
simple_title:         "Leggere un file di testo"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/python/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Perché leggere un file di testo?

Leggere un file di testo può essere utile in molte situazioni, come ad esempio quando si vogliono analizzare i dati contenuti in un documento o quando si vuole elaborare un testo in modo automatizzato. Grazie al linguaggio Python, è possibile leggere e manipolare facilmente i contenuti di un file di testo.

## Come leggere un file di testo in Python

Per leggere un file di testo in Python, è necessario seguire questi semplici passaggi:

1. Aprire il file utilizzando la funzione `open()` specificando il percorso del file e la modalità di apertura desiderata. Ad esempio, `open("test.txt", "r")` apre il file "test.txt" in modalità di lettura.

2. Leggere il contenuto del file utilizzando il metodo `read()` oppure utilizzando un ciclo `for` per leggere il file riga per riga.

3. Chiudere il file utilizzando il metodo `close()` per liberare le risorse del sistema.

Ecco un esempio di codice che legge un file di testo e stampa il suo contenuto su schermo utilizzando un ciclo `for`:

```Python
with open("test.txt", "r") as file:
    for line in file:
        print(line)
```

L'output di questo codice sarà il seguente:

```
Ciao!
Questo è un file di testo.
Spero che ti stia trovando bene.
```

## Approfondimento sulla lettura di un file di testo

Ci sono alcune cose importanti da tenere a mente quando si legge un file di testo in Python:

- La funzione `open()` restituisce un oggetto file, quindi è possibile assegnarlo ad una variabile per riferirsi al file in modo più semplice.

- È importante specificare la modalità di apertura del file corretta. La modalità di lettura `r` è la più comune, ma ci sono anche altre modalità disponibili per esempio per scrivere o aggiungere al file.

- Per leggere il contenuto di un file in una sola stringa, si può utilizzare il metodo `read()` invece di un ciclo `for`.

## Vedi anche

- [Documentazione ufficiale di Python su file input/output](https://docs.python.org/3/tutorial/inputoutput.html#reading-and-writing-files)
- [Articolo su Real Python su lettura e scrittura di file di testo](https://realpython.com/read-write-files-python/)
- [Esempi di codice su GitHub per leggere file di testo in Python](https://github.com/topics/read-text-file-python)