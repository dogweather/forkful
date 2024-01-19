---
title:                "Lettura degli argomenti della riga di comando"
html_title:           "Java: Lettura degli argomenti della riga di comando"
simple_title:         "Lettura degli argomenti della riga di comando"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/python/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Che cosa & Perché?

Leggere gli argomenti della riga di comando significa accettare gli input direttamente dal terminale o console. Lo facciamo per permettere agli utenti di personalizzare l'esecuzione del programma senza modificare il codice.

## Come fare:

In Python, `sys.argv` è una lista in Python che contiene gli argomenti della riga di comando. Ecco un esempio semplice:

```Python
import sys

# Stampa tutti gli argomenti
for i in range(len(sys.argv)):
    print(f"Argomento {i}: {sys.argv[i]}")
```
Se salvi il codice sopra come `args.py` e lo esegui da una riga di comando con `python args.py arg1 arg2 arg3`, otterrai:

```
Argomento 0: args.py
Argomento 1: arg1
Argomento 2: arg2
Argomento 3: arg3
```

## Approfondimento

Nonostante `sys.argv` sia il metodo più popolare, non era l'unico disponibile quando Python fu rilasciato per la prima volta. La libreria `getopt` era quella più usata nelle prime versioni di Python. Oggi, `argparse` è un alternativa più moderna e potente, che offre validazione avanzata degli argomenti e generazione automatica del messaggio di utilizzo.

`sys.argv` include sempre il nome dello script come suo primo elemento (indice 0). Gli argomenti successivi, inseriti dall'utente, seguono l'ordinamento.

## Per approfondire

1. Documentazione di Python su `sys.argv`: https://docs.python.org/it/3/library/sys.html#sys.argv
2. Documentazione di Python su `argparse`: https://docs.python.org/it/3/library/argparse.html
3. Resoconto storico su `getopt`: https://www.gnu.org/software/libc/manual/html_node/Getopt.html 
4. Guida pratica sull'uso di `argparse` in Python: https://realpython.com/command-line-interfaces-python-argparse/
5. Versione leggera della libreria `argparse` per script più piccoli e semplici: https://docs.python.org/it/3/library/argparse.html#argparse.ArgumentParser