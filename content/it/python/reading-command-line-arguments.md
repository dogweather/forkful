---
title:                "Lettura degli argomenti della riga di comando"
html_title:           "Python: Lettura degli argomenti della riga di comando"
simple_title:         "Lettura degli argomenti della riga di comando"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/python/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Perché

So che probabilmente il titolo di questo articolo ti ha fatto storcere il naso un po'. Command line arguments? Cosa diavolo sono? Non preoccuparti, non è nulla di complicato. In poche parole, i command line arguments sono delle variabili che puoi passare al tuo programma quando lo esegui da riga di comando. Ad esempio, quando digiti "python programma.py arg1 arg2" stai passando due command line arguments al programma "programma.py". Ora, perché dovresti preoccuparti di questo? Beh, continua a leggere e lo scoprirai.

## Come Fare

E' molto semplice leggere i command line arguments in Python. Ti basterà utilizzare la libreria "sys" e la funzione "argv". Ecco un esempio:

```Python
import sys

# Legge il primo command line argument
arg1 = sys.argv[1]

# Legge il secondo command line argument
arg2 = sys.argv[2]

# Stampa i valori dei due argument
print(arg1) # Output: arg1
print(arg2) # Output: arg2
```

Come puoi vedere, la funzione "argv" restituisce una lista di tutti i command line arguments passati al programma, inclusi il nome del programma stesso. Questo significa che dovrai considerare l'indice di ogni argument all'interno della lista in base all'ordine in cui sono stati passati.

## Deep Dive

Ora che sai come leggere i command line arguments, potresti chiederti perché dovresti farlo. In realtà, ci sono diverse situazioni in cui questa abilità può essere molto utile. Ad esempio, potresti voler creare un programma che si comporti in modo diverso in base agli argument passati, o magari potresti voler automatizzare alcune operazioni utilizzando gli argument come input. Inoltre, i command line arguments possono essere anche utilizzati per passare informazioni sensibili, come username e password, in modo sicuro.

## Vedi Anche

- [Documentazione ufficiale sul modulo sys](https://docs.python.org/3/library/sys.html)
- [Tutorial su come utilizzare i command line arguments in Python](https://realpython.com/python-command-line-arguments/)
- [Esempi pratici di utilizzo dei command line arguments](https://www.pythonforbeginners.com/system/python-sys-argv)