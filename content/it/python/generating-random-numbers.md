---
title:                "Python: Generazione di numeri casuali"
programming_language: "Python"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/python/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Perché

Molte volte nella programmazione abbiamo bisogno di utilizzare numeri casuali per simulare situazioni reali o per generare dati casuali per i nostri algoritmi. In Python, possiamo farlo facilmente utilizzando le funzioni per la generazione di numeri casuali.

## Come si fa

Per generare numeri casuali in Python, dobbiamo prima importare il modulo "random". Possiamo farlo utilizzando la seguente riga di codice:

```python
import random
```

Una volta importato il modulo, possiamo utilizzare la funzione "random.random()" per generare un numero casuale compreso tra 0.0 e 1.0. Ad esempio, se vogliamo generare un numero casuale intero compreso tra 1 e 100, possiamo utilizzare la funzione "random.randint(1,100)".

```python
import random
numero_casuale = random.randint(1,100)
print(numero_casuale) #output: un numero casuale tra 1 e 100
```

Possiamo anche creare una lista di numeri casuali utilizzando la funzione "random.sample(seq, k)" dove "seq" è la sequenza di numeri e "k" è il numero di elementi da scegliere. Ad esempio, se abbiamo una lista di 10 numeri e vogliamo estrarre 5 numeri casualmente da essa, possiamo utilizzare la seguente riga di codice:

```python
numeri = [1,2,3,4,5,6,7,8,9,10]
numeri_casuali = random.sample(numeri, 5)
print(numeri_casuali) #output: una lista di 5 numeri casuali presi dalla lista "numeri"
```

## Approfondimento

La generazione di numeri casuali non è un processo completamente casuale. In realtà, si basa su un algoritmo che utilizza un valore iniziale detto "seed" per calcolare il numero casuale successivo. Questo significa che se forniamo lo stesso seed, otterremo sempre lo stesso numero casuale. Possiamo anche impostare il seed manualmente utilizzando la funzione "random.seed(x)" dove "x" è il nostro valore desiderato per il seed.

Inoltre, il modulo "random" in Python offre anche altre funzioni che ci permettono di scegliere numeri casualmente da una distribuzione specifica come la distribuzione normale o la distribuzione esponenziale.

## Vedi anche
- [Documentazione ufficiale del modulo random in Python](https://docs.python.org/3/library/random.html)
- [Tutorial sulla generazione di numeri casuali in Python](https://realpython.com/python-random/)
- [Esempi di utilizzo dei numeri casuali in Python](https://www.geeksforgeeks.org/generating-random-number-list-in-python/)