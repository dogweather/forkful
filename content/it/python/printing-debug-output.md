---
title:                "Python: Stampa dell'output di debug"
simple_title:         "Stampa dell'output di debug"
programming_language: "Python"
category:             "Python"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/python/printing-debug-output.md"
---

{{< edit_this_page >}}

## Perché

Stampare output di debug è una pratica molto importante nel processo di sviluppo di un programma. Aiuta a identificare e risolvere eventuali errori o bug nel codice, rendendo il processo di debugging più efficiente e accurato.

## Come fare

Per stampare output di debug in Python, è possibile utilizzare il metodo `print()` oppure il modulo `logging`. Vediamo un esempio di utilizzo di entrambi:

```
# Utilizzo del metodo print()
numero = 7
print("Il numero è:", numero)
```
Output:
```
Il numero è: 7
```

```
# Utilizzo del modulo logging
import logging
logging.basicConfig(level=logging.DEBUG)
numero = 7
logging.debug("Il numero è: %d", numero)
```
Output:
```
DEBUG:root:Il numero è: 7
```

In entrambi i casi, l'output di debug verrà visualizzato nella console o nel file di log a seconda delle impostazioni.

## Approfondimento

Stampare output di debug può essere utile non solo per identificare errori, ma anche per comprendere meglio il funzionamento del codice. Si consiglia di utilizzare il metodo `print()` inizialmente per accertarsi di avere la giusta logica nel codice e successivamente passare al modulo `logging` per avere un controllo più dettagliato e strutturato dell'output di debug.

Inoltre, è possibile utilizzare stringhe di formattazione come `%s` o `%d` per visualizzare variabili di diversi tipi di dati nell'output di debug.

## Vedi anche

- [Python Documentazione ufficiale su print()](https://docs.python.org/3/library/functions.html#print)
- [Python Documentazione ufficiale su logging](https://docs.python.org/3/library/logging.html)
- [Python Guida al debugging](https://realpython.com/python-debugging-pdb/)