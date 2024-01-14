---
title:                "Python: Leggere un file di testo"
programming_language: "Python"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/python/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Perché
Lettura di un file di testo è un'operazione fondamentale nella programmazione Python. Se vuoi manipolare dati da file come CSV o JSON, o semplicemente leggere un file di testo normale, questo è un passaggio fondamentale per la tua applicazione.

## Come fare
Per prima cosa, dovrai aprire il file di testo utilizzando la funzione `open()`. Assicurati di specificare il percorso completo del file e il modo in cui vuoi aprirlo (lettura, scrittura, ecc.). Ad esempio, per aprire un file di testo in lettura, puoi usare questo codice:

```Python
file = open("testo.txt", "r")
```

Una volta aperto il file, puoi leggerne il contenuto con il metodo `read()`. Questo restituirà l'intero contenuto del file come una stringa. Puoi anche specificare il numero di caratteri che vuoi leggere utilizzando `read(n)`, dove `n` è il numero di caratteri desiderati. Ad esempio, per leggere solo i primi 100 caratteri del file, puoi usare questo codice:

```Python
contenuto = file.read(100)
```

Puoi anche utilizzare un ciclo `for` per leggere il contenuto del file linea per linea. In questo caso, utilizziamo il metodo `readline()` che restituisce ogni riga del file come una stringa. Ad esempio:

```Python
for riga in file.readline():
  print(riga)
```

Infine, non dimenticare di chiudere il file utilizzando il metodo `close()` una volta finito di leggerlo.

## Approfondimento
Esistono anche altre funzioni utili per la lettura di file di testo, come ad esempio `readlines()` che restituisce una lista contenente ogni riga del file come un elemento, o `seek()`, che viene utilizzata per spostarsi in una posizione specifica del file.

Inoltre, è possibile specificare l'encoding del file durante l'apertura utilizzando il parametro `encoding` nella funzione `open()`. Ciò può essere utile per leggere correttamente file con caratteri o simboli speciali.

## Vedi anche
- [Documentazione Python - File Objects](https://docs.python.org/3.8/tutorial/inputoutput.html#reading-and-writing-files)
- [Tutorialspoint - Python File I/O](https://www.tutorialspoint.com/python/python_files_io.htm)
- [Real Python - Reading and Writing Files in Python](https://realpython.com/read-write-files-python/#the-basics-reading-a-file)