---
title:    "Python: Lettura degli argomenti della linea di comando."
keywords: ["Python"]
---

{{< edit_this_page >}}

## Perché leggere gli argomenti della riga di comando in Python

Python è un linguaggio di programmazione versatile e potente, e una delle sue caratteristiche più interessanti è la possibilità di leggere gli argomenti della riga di comando quando si avvia uno script. Questi argomenti sono delle variabili che l'utente può inserire al momento dell'esecuzione dello script per influenzarne il comportamento. Vediamo perché questo è importante e come farlo in maniera efficace.

## Come leggere gli argomenti della riga di comando in Python

Per leggere gli argomenti della riga di comando in Python, possiamo utilizzare il modulo `sys` e la variabile `argv`. Iniziamo importando il modulo:

```Python
import sys
```

Poi, possiamo utilizzare `sys.argv` per accedere alla lista degli argomenti passati dalla riga di comando:

```Python
arguments = sys.argv
```

Possiamo inoltre utilizzare la funzione `len()` per determinare il numero di argomenti passati, e l'operatore `[]` per accedere ai singoli argomenti in base al loro indice:

```Python
num_arguments = len(arguments)
first_argument = arguments[0]
second_argument = arguments[1]
```

Ora, quando eseguiamo il nostro script dalla riga di comando, possiamo passare degli argomenti e accedervi all'interno del codice. Ad esempio, se il nostro script si chiama `hello.py` e vogliamo passare il nostro nome come argomento, possiamo scrivere:

```Python
python hello.py Marco
```

E all'interno del nostro script, possiamo accedere all'argomento come `sys.argv[1]`:

```Python
import sys

name = sys.argv[1]
print("Ciao", name)
```

L'output del nostro script sarebbe:

```
Ciao Marco
```

## Approfondimento sui comandi della riga di comando in Python

Oltre ad accedere agli argomenti passati dalla riga di comando, esistono altre funzionalità per gestirli in maniera più avanzata. Ad esempio, possiamo utilizzare il modulo `argparse` per definire argomenti obbligatori e opzionali, opzioni, e addirittura delle descrizioni e delle istruzioni di aiuto per gli utenti.

Inoltre, è possibile fare una validazione degli argomenti passati e gestirne gli errori, garantendo che il nostro script abbia sempre un input valido.

## Vedi anche

- [Documentazione ufficiale di Python su sys.argv](https://docs.python.org/3/library/sys.html#sys.argv)
- [Guida su argparse in Python](https://realpython.com/command-line-interfaces-python-argparse/)