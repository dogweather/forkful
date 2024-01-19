---
title:                "Stampa dell'output di debug"
html_title:           "Bash: Stampa dell'output di debug"
simple_title:         "Stampa dell'output di debug"
programming_language: "Python"
category:             "Python"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/python/printing-debug-output.md"
---

{{< edit_this_page >}}

## Cos'è e perché?

Il debug, o produzione di output di debug, consiste nell'uscita di informazioni dettagliate relative all'esecuzione di un programma. I programmatori lo fanno per identificare e risolvere qualsiasi problema che potrebbe ostacolare il funzionamento del programma.

## Come fare:

Python offre varie opzioni per il debug di un programma.

```Python
# Usare print() per il debug:
a = 10
b = 20
print(f"I valori delle variabili sono - a: {a}, b: {b}")
```
Output:

    I valori delle variabili sono - a: 10, b: 20

Un altro modo è utilizzare il modulo logging:

```Python
import logging

# Crea e configura il logger
logging.basicConfig(level=logging.DEBUG)

# Utilizzare logging.debug()

a = 10
b = 20
logging.debug(f"I valori delle variabili sono - a: {a}, b: {b}")
```

Questo non produrrà alcun output fino a quando il livello di logging non viene impostato su DEBUG.

## Approfondimento:

La pratica di stampare l'output di debug ha radici storiche, essendo uno dei metodi più antichi e di base per il debug del codice. Pur essendo semplice, è ancora un mezzo efficace per comprendere il flusso di esecuzione di un programma.

Esistono alternative più raffinate come l'uso di debugger integrati in IDE come PyCharm, che offrono un controllo granulare sull'esecuzione del codice. Tuttavia, nulla batte la semplicità e l'accessibilità di un buon vecchio print statement.

Per quanto riguarda i dettagli di implementazione, `print()` e `logging.debug()` sono funzioni di libreria standard in Python. La differenza principale è che `print()` scrive sempre l'output sulla console, mentre l'output di `logging.debug()` può essere configurato per scrivere l'output su diversi luoghi, come file, console, ecc., e ha diversi livelli.

## Vedi anche:

Per maggiori informazioni, consulta le seguenti risorse:
- [Python's official documentation on the print function](https://docs.python.org/3/library/functions.html#print)
- [Python's official documentation on logging](https://docs.python.org/3/library/logging.html)
- [Python Debugging Techniques](https://realpython.com/python-debugging/)