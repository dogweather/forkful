---
title:                "Lettura degli argomenti della riga di comando"
aliases: - /it/python/reading-command-line-arguments.md
date:                  2024-01-20T17:56:30.641828-07:00
model:                 gpt-4-1106-preview
simple_title:         "Lettura degli argomenti della riga di comando"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/python/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why?
Leggere gli argomenti della riga di comando significa estrarre i parametri che passi al tuo script Python al momento dell'esecuzione. I programmatori usano questa tecnica per personalizzare l'esecuzione del programma senza cambiare il codice sorgente.

## How to:
Usiamo `sys.argv` per accedere agli argomenti. È semplice. Ecco un esempio:

```Python
import sys

if len(sys.argv) > 1:
    print(f"Ciao, {sys.argv[1]}!")
else:
    print("Ciao, mondo!")
```

Se esegui `python script.py Mario`, otterrai:

```
Ciao, Mario!
```

Se lanci solo `python script.py`:

```
Ciao, mondo!
```

## Deep Dive
`sys.argv` è un elenco, semplice e senza fronzoli. Il primo elemento, `sys.argv[0]`, è il nome dello script. Ogni argomento dopo è uno stringa inserita dall'utente. 

Historically, leggere gli argomenti della riga di comando è una pratica antica, risalente ai primi giorni di UNIX. Python offre altre opzioni come `argparse` per argomenti più complessi, e `os.environ` per leggere le variabili d'ambiente.

A proposito di implementazione, `sys.argv` funziona bene per casi d'uso semplici. Quando le tue necessità crescono, considera `argparse` che supporta flag, opzioni predefinite e aiuto automatico.

## See Also
- Per approfondire `argparse`, vedi la [documentazione ufficiale](https://docs.python.org/3/library/argparse.html).
- Lo [Standard Library Tutorial](https://docs.python.org/3/tutorial/stdlib.html#command-line-arguments) è utile per imparare gli usi di base di `sys.argv`.
- Guarda [questo video](https://www.youtube.com/watch?v=CqvZ3vGoGs0) per un tutorial visivo su `sys.argv` e `argparse`.
