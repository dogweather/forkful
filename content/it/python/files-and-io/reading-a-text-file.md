---
date: 2024-01-20 17:54:59.837862-07:00
description: How to (Come Fare) Ecco come leggere un file di testo in Python.
lastmod: '2024-04-05T22:38:42.508002-06:00'
model: gpt-4-1106-preview
summary: How to (Come Fare) Ecco come leggere un file di testo in Python.
title: Lettura di un file di testo
weight: 22
---

## How to (Come Fare)
Ecco come leggere un file di testo in Python:

```Python
# Lettura di tutto il contenuto del file
with open('esempio.txt', 'r') as file:
    contenuto = file.read()
    print(contenuto)

# Lettura linea per linea
with open('esempio.txt', 'r') as file:
    for linea in file:
        print(linea.strip())  # strip() rimuove gli a capo
```

Esempio di output:

```
Prima riga del file
Seconda riga del file
```

## Deep Dive (Approfondimento)
In passato, leggere un file in Python richiedeva gestire direttamente la memoria e la chiusura del file. Dal Python 2.5, è consigliato usare il `with` statement, che chiude automaticamente il file dopo l'uso.

Alternative esistono. Ad esempio, `file.readlines()` restituisce una lista di righe. O, con `file.readline()` si legge riga per riga, utile per file molto grandi. Da Python 3.8, è possibile usare anche il modulo `pathlib` per un approccio orientato agli oggetti.

Dettagli implementativi includono il trattare l'encoding dei file (`'utf-8'` è comunemente usato) e la gestione di errori, come i file non trovati, utilizzando il blocco `try...except`.

## See Also (Vedi Anche)
- Documentazione Python su file I/O: https://docs.python.org/3/tutorial/inputoutput.html#reading-and-writing-files
- `pathlib` per lavorare con i filesystem paths: https://docs.python.org/3/library/pathlib.html
- Guida Python su `with` statement: https://docs.python.org/3/reference/compound_stmts.html#with
