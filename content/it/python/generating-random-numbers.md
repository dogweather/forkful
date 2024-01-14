---
title:                "Python: Generazione di numeri casuali"
simple_title:         "Generazione di numeri casuali"
programming_language: "Python"
category:             "Python"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/python/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Perché

Generare numeri casuali è un'attività molto utile in programmazione Python, perché permette di simulare situazioni casuali e aggiungere una componente di casualità ai nostri programmi. Inoltre, è uno strumento fondamentale per la creazione di giochi, test e algoritmi di ottimizzazione.

## Come fare

Per generare numeri casuali in Python, possiamo utilizzare il modulo "random". Basta importare il modulo all'inizio del nostro codice e utilizzare la funzione "randint()". Ad esempio, se vogliamo generare un numero casuale compreso tra 1 e 10, possiamo scrivere:

```python
import random
numero_casuale = random.randint(1, 10)
print(numero_casuale)
```
Questo produrrà un output simile a questo: 5

Possiamo anche utilizzare la funzione "choice()", che ci permette di selezionare un elemento casuale da una lista. Ad esempio, se abbiamo una lista di nomi e vogliamo scegliere uno a caso, possiamo scrivere:

```python
nomi = ["Marco", "Giulia", "Luca", "Sara"]
nome_casuale = random.choice(nomi)
print(nome_casuale)
```
Questo produrrà un output simile a questo: Giulia.

## Approfondimento

Il modulo "random" utilizza un algoritmo chiamato "Mersenne Twister" per generare i numeri casuali. Questo algoritmo è in grado di produrre una lunga sequenza di numeri casuali senza ripetizioni, garantendo la casuale rappresentatività dei risultati.

Inoltre, Python dispone anche di altri moduli per la generazione di numeri casuali, come "secrets" per generare numeri sicuri o "numpy" per la creazione di matrici di numeri casuali.

## Vedi anche

- Documentazione ufficiale su come generare numeri casuali in Python: https://docs.python.org/3/library/random.html
- Esempi di utilizzo del modulo "random": https://www.programiz.com/python-programming/random-numbers
- Approfondimenti sull'algoritmo "Mersenne Twister": https://qr.ae/pGSbFh