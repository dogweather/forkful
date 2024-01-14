---
title:                "Python: Leggere gli argomenti della riga di comando"
simple_title:         "Leggere gli argomenti della riga di comando"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/python/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Perché

Scrivere un programma può essere un processo lungo e complesso, ma a volte è necessario essere in grado di interagire con esso a un livello più granulare. Una delle maniere principali di farlo è attraverso l'utilizzo degli argomenti della riga di comando. Questo articolo esplorerà il perché di questa pratica e come implementarla con Python.

## Come Fare

Per leggere gli argomenti della riga di comando in Python, è necessario utilizzare il modulo `sys` che contiene una lista `argv` che rappresenta gli argomenti passati al programma. Vediamo un esempio di codice che stampa gli argomenti passati in un formato leggibile:

```
Python
import sys

args = sys.argv

for arg in args:
    print("Argomento:", arg)
```

Se lanciassimo questo programma con il comando `python my_prog.py hello world`, vedremo il seguente output:

```
Argomento: my_prog.py
Argomento: hello
Argomento: world
```

Come possiamo notare, il nome del file stesso è incluso nella lista degli argomenti. Per escluderlo, possiamo utilizzare il metodo `pop()` per rimuoverlo dalla lista. Inoltre, possiamo passare argomenti con uno spazio nel loro interno utilizzando virgolette sul terminale.

## Deep Dive

Oltre alla semplice lettura degli argomenti passati al programma, è possibile utilizzare il modulo `argparse` per definire degli argomenti specifici che il programma deve accettare. Questo rende il codice più leggibile e permette all'utente di avere un'interazione più intuitiva con il programma.

Ad esempio, possiamo definire un argomento opzionale '--language' utilizzando `argparse` e assegnare un valore di default:

```
Python
import argparse

parser = argparse.ArgumentParser()
parser.add_argument("--language", help="Seleziona la lingua desiderata", default="italiano")

args = parser.parse_args()

print("La lingua selezionata è:", args.language)
```

Se lanciassimo questo programma con il comando `python my_prog.py --language inglese`, vedremo il seguente output:

```
La lingua selezionata è: inglese
```

Tuttavia, se non passiamo alcun argomento, verrà utilizzata il valore di default "italiano". È anche possibile definire argomenti obbligatori, argomenti che accettano più valori e molti altri opzioni.

## Vedi Anche

Per ulteriori informazioni sull'utilizzo degli argomenti della riga di comando in Python, ti consigliamo di consultare la documentazione ufficiale sul modulo `sys` e `argparse`.

- [Documentazione ufficiale di Python sul modulo sys](https://docs.python.org/3/library/sys.html)
- [Documentazione ufficiale di Python sul modulo argparse](https://docs.python.org/3/library/argparse.html)