---
title:                "Generazione di numeri casuali"
html_title:           "Arduino: Generazione di numeri casuali"
simple_title:         "Generazione di numeri casuali"
programming_language: "Python"
category:             "Python"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/python/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Cosa e Perché?

Generare numeri casuali consiste nell'ottenere numeri che non seguono alcun schema, che sono completamente imprevisti. Questo è utilizzato dai programmatori per vari scopi come simulazioni, testing, giocosità, e per garantire sicurezza nei dati crittografati.

## Come fare:

Per generare numeri casuali in Python, si usa la libreria built-in `random`. Vediamo alcuni esempi.

```Python
import random

# Genera un numero casuale tra 0 e 1
numero_casuale = random.random()
print(numero_casuale)

# Genera un numero intero casuale tra 1 e 10
numero_intero_casuale = random.randint(1, 10)
print(numero_intero_casuale)
```

L'esecuzione del codice di cui sopra genererà qualcosa di simile a questo:

```
0.345678912459
8
```

## Approfondimento

La generazione di numeri casuali ha una lunga storia nella programmazione. Nonostante sia impossibile generare veri numeri casuali con un calcolatore, c'è un campo intero dedicato alla generazione di numeri pseudo-casuali che appaiono casuali per la maggior parte degli usi pratici. 

Esistono diverse alternative per generare numeri casuali in Python, come l'uso della funzione `random()` nel modulo `numpy`. Questa funzione è particolarmente utile quando si lavora con array o quando si generano grandi quantità di numeri casuali.

Il modulo `random` di Python utilizza l'algoritmo Mersenne Twister per generare numeri pseudo-casuali. Ha un periodo molto lungo di 2^19937-1 itereazioni, il che significa che la sequenza di numeri casuali si ripeterà solo dopo tante iterazioni.

## Vedi anche

Per ulteriori informazioni sulla generazione di numeri casuali in Python e sulla sua storia, consulta i seguenti link:
 
1. Documentazione ufficiale Python su `random`: https://docs.python.org/3/library/random.html
2. Wikipedia sull'Algoritmo Mersenne Twister: https://it.wikipedia.org/wiki/Mersenne_twister
3. Documentazione ufficiale su `numpy.random`: https://numpy.org/doc/stable/reference/random/index.html