---
title:                "Generare numeri casuali"
html_title:           "Python: Generare numeri casuali"
simple_title:         "Generare numeri casuali"
programming_language: "Python"
category:             "Python"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/python/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Perché

Generare numeri casuali può essere utile in molteplici contesti, come ad esempio nel testing di algoritmi, nella simulazione di situazioni casuali o nella creazione di giochi.

## Come fare

Per generare numeri casuali in Python, è possibile utilizzare il modulo `random` che offre una vasta gamma di funzioni. Ecco alcuni esempi di come utilizzare il modulo per ottenere numeri casuali:

```
import random

# Genera un numero intero casuale tra 0 e 10
random.randint(0, 10)
# Output: 7

# Genera un numero decimale casuale tra 0 e 1
random.random()
# Output: 0.523446

# Genera una lista di 5 numeri casuali tra 1 e 100
random.sample(range(1, 100), 5)
# Output: [54, 76, 12, 31, 89]
```

Utilizzando queste funzioni, è possibile ottenere numeri casuali di diversi tipi e in diversi intervalli.

## Approfondimento

La generazione di numeri casuali in Python è basata sull'algoritmo "Mersenne Twister" che utilizza un seed (seme) per generare sequenze di numeri apparentemente casuali. Ciò significa che, utilizzando lo stesso seed, verranno generati sempre gli stessi numeri casuali. Per questo motivo, è importante impostare un seed diverso ogni volta che si vuole ottenere una nuova sequenza di numeri.

Inoltre, è importante tenere presente che i numeri casuali generati in questo modo non sono veramente casuali, ma sono deterministicamente calcolati dall'algoritmo. Questi numeri possono essere utili in molte situazioni, ma non devono essere considerati sicuri per applicazioni di crittografia o sicurezza.

## Vedi anche
- [Documentazione ufficiale del modulo `random` in Python](https://docs.python.org/3/library/random.html)
- [Tutorial su come utilizzare il modulo `random` in Python](https://realpython.com/python-random/)
- [Ulteriori informazioni sul concetto di numeri pseudo-casuali](https://en.wikipedia.org/wiki/Pseudorandom_number_generator)