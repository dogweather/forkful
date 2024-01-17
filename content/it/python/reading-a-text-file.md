---
title:                "Lettura di un file di testo."
html_title:           "Python: Lettura di un file di testo."
simple_title:         "Lettura di un file di testo."
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/python/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
Leggere un file di testo è un'operazione comune per i programmatori. Essenzialmente, significa ottenere il contenuto di un file di testo e utilizzarlo all'interno del nostro programma. Ciò può essere fatto per diversi motivi, come ad esempio l'elaborazione dei dati, l'analisi dei testi o per creare report.

## Come fare:
Per prima cosa, dobbiamo aprire il file di testo utilizzando la funzione `open ()`, specificando il nome del file e la modalità di apertura (lettura, scrittura, append). Successivamente, possiamo utilizzare diversi metodi per leggere il contenuto del file, come ad esempio `read()`, `readline()` o `readlines()`. Ad esempio:

```Python
file = open("test.txt", "r")
print(file.read())
```
Output:
```
Ciao a tutti! Questo è un esempio di file di testo che può essere letto con Python.
```

## Approfondimento:
La lettura di un file di testo ha un'importante storia nell'informatica, in quanto è stata una delle prime forme di input/output utilizzate dai computer. Inoltre, oltre al metodo `read()` visto precedentemente, esistono altre opzioni per ottenere il testo da un file, come l'utilizzo delle espressioni regolari o delle librerie specifiche per il parsing dei dati.

## Vedi anche:
- [Python File Input/Output](https://docs.python.org/3/tutorial/inputoutput.html)
- [Python Regular Expressions](https://docs.python.org/3/library/re.html)