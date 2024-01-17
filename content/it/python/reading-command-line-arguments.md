---
title:                "Leggere gli argomenti dalla riga di comando"
html_title:           "Python: Leggere gli argomenti dalla riga di comando"
simple_title:         "Leggere gli argomenti dalla riga di comando"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/python/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Cosa e perché?
Lettura degli argomenti della riga di comando è quando un programma Python prende i dati direttamente dalla riga di comando, dopo il nome del programma stesso. I programmatori lo fanno per rendere il loro codice più flessibile e versatile, permettendo loro di passare dati diversi al programma ogni volta che viene eseguito.

## Come fare:
Vediamo un esempio semplice di come leggere gli argomenti della riga di comando in Python:

```Python
import sys

args = sys.argv

# Il primo argomento è sempre il nome del programma stesso, quindi saltiamolo e prendiamo il secondo argomento
numero = int(args[1])

# Stampa il quadrato del numero passato come argomento
print(numero * numero)

# Esegui questo codice da riga di comando con "python nome_programma.py 5" dove "5" è il numero da elevare al quadrato
```

Output:
```
25
```

## Approfondimento:
La lettura degli argomenti della riga di comando risale ai primi giorni della programmazione quando i computer non avevano ancora una GUI. In alternativa, i programmatori potrebbero utilizzare variabili "hard-coded" nel codice, ma questo lo renderebbe meno flessibile e più difficile da modificare in futuro. Implementare la lettura degli argomenti della riga di comando richiede una comprensione dei concetti di sys.argv e il loro utilizzo in condizioni di loop e cicli per gestire più argomenti.

## Vedi anche:
- [Documentazione ufficiale di Python su sys.argv](https://docs.python.org/3/library/sys.html#sys.argv)
- [Esempi di codice per la lettura degli argomenti della riga di comando su GitHub](https://github.com/topics/reading-command-line-arguments?l=python)