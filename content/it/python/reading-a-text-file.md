---
title:                "Lettura di un file di testo"
html_title:           "C: Lettura di un file di testo"
simple_title:         "Lettura di un file di testo"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/python/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Che Cos'è & Perché?
La lettura di un file di testo consiste nell'aprire e leggere il contenuto di un file in formato di testo utilizzando un programma o un linguaggio di programmazione. Questa operazione è utile per i programmatori poiché permette la manipolazione e l'elaborazione delle informazioni contenute nel file. 

## Come si fa:
Ecco un esempio su come leggere un file di testo in Python:
```Python
with open('miofile.txt', 'r') as file:
    contenuto = file.read()
print(contenuto)
```
Se il file 'miofile.txt' contiene il testo "Ciao, mondo!", l'output sarà:
```Python
Ciao, mondo!
```

## Approfondimento
La capacità di leggere i file di testo esiste sin dalle prime versioni dei linguaggi di programmazione. È una caratteristica essenziale e molto utilizzata, ad esempio, per il recupero di informazioni da file di log o database.

Ci sono molte alternative per leggere un file di testo in Python. Due di queste sono: la lettura del file riga per riga con `file.readline()`, o leggere tutto il file in una lista con `file.readlines()`.

Nel contesto dell'implementazione, quando apriamo un file con `open()`, Python crea un oggetto file che fornisce metodi e attributi necessari per leggere, salvare e manipolare il file.

## Vedi Anche
- [Documentazione ufficiale Python su come leggere e scrivere file](https://docs.python.org/3/tutorial/inputoutput.html#reading-and-writing-files)
- [W3Schools: Python File Handling](https://www.w3schools.com/python/python_file_handling.asp)