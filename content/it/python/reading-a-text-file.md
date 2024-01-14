---
title:    "Python: Lettura di un file di testo"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/python/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Perché

Scrivere un programma in Python può sembrare scoraggiante per alcuni, ma non preoccuparti! Leggere un file di testo non è così difficile come sembra. In questo articolo, esploreremo come leggere un file di testo utilizzando Python.

## Come fare

Per iniziare, avrai bisogno di un file di testo che desideri leggere. Assicurati che sia presente nella stessa cartella del tuo codice Python. Iniziamo con l'apertura del file utilizzando la funzione `open ()` e assegnandola a una variabile:

```Python
file = open("mio_file.txt")
```

Una volta che il file è stato aperto, possiamo utilizzare il metodo `read()` per leggere tutto il contenuto del file e assegnarlo ad una variabile:

```Python
contenuto = file.read()
```

Ora possiamo stampare il contenuto del file utilizzando la funzione `print()`:

```Python
print(contenuto)
```

Se vogliamo leggere solo un certo numero di caratteri dal file, possiamo utilizzare il metodo `readlines()` e specificare il numero di caratteri come argomento all'interno delle parentesi:

```Python
finestra = file.readlines(10)
print(finestra)
```

Questo stampa solo i primi 10 caratteri del file. Una volta che abbiamo finito di leggere il file, è importante chiuderlo utilizzando il metodo `close()`:

```Python
file.close()
```

## Approfondimento

Oltre ai metodi `read()` e `readlines()`, ci sono anche altri modi per leggere un file di testo utilizzando Python. Ad esempio, possiamo utilizzare il ciclo `for` per leggere riga per riga il contenuto del file:

```Python
file = open("mio_file.txt")

for riga in file:
  print(riga)

file.close()
```

Inoltre, possiamo specificare il parametro `encoding` nella nostra funzione `open()` se vogliamo leggere un file di testo con una codifica diversa da quella predefinita:

```Python
file = open("mio_file.txt", encoding="utf-16")
```

Ricorda sempre di chiudere il file una volta terminato!

## Vedi anche

Se vuoi saperne di più su come leggere e scrivere file di testo in Python, ecco alcuni link utili:

- [Documentazione di Python su File I/O](https://docs.python.org/3/tutorial/inputoutput.html#reading-and-writing-files)
- [Tutorial su Real Python](https://realpython.com/read-write-files-python/)
- [Video tutorial su YouTube di Corey Schafer](https://www.youtube.com/watch?v=Uh2ebFW8OYM)

Buona lettura!