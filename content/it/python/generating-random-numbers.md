---
title:    "Python: Creazione di numeri casuali"
keywords: ["Python"]
---

{{< edit_this_page >}}

## Perché
Le random numbers o numeri casuali sono uno strumento importante nella programmazione Python. Questi numeri possono essere utili per creare dati casuali per testare il nostro codice o per aggiungere un elemento di casualità ai nostri programmi.

## Come fare
Esistono diverse metodologie per generare random numbers in Python. Una delle più comuni è l'utilizzo del modulo integrato "random". Possiamo utilizzare la funzione "randint()" per generare un numero intero casuale all'interno di un intervallo specificato, oppure la funzione "random()" per ottenere un numero decimale casuale compreso tra 0 e 1. Ecco un esempio di codice che genera 5 numeri interi casuali compresi tra 1 e 10 e li stampa a schermo:

```Python
import random

for i in range(5):
    print(random.randint(1,10))
```

Output:
```
2
7
4
9
10
```

## Approfondimento
La generazione di numeri casuali non è un processo completamente randomico, ma si basa su algoritmi matematici predefiniti. Questo significa che non sono veramente casuali, ma piuttosto sono determinati da un seed iniziale che funge da input per l'algoritmo. Per tale motivo, se impostiamo lo stesso seed, otterremo sempre gli stessi numeri casuali.

Per migliorare la casualità dei numeri generati, possiamo utilizzare la funzione "seed()" per impostare il seed in modo casuale in base al tempo passato dall'ultima mezzanotte. In alternativa, possiamo utilizzare il modulo "secrets" per creare numeri casuali basati sull'entropia del sistema operativo.

## Vedi anche
Per ulteriori informazioni sulla generazione di numeri casuali in Python, puoi consultare i seguenti link:

- Documentazione ufficiale sul modulo "random" di Python: https://docs.python.org/3/library/random.html
- Guida all'utilizzo del modulo "secrets": https://www.python.org/dev/peps/pep-0506/#example-code
- Un approfondimento sulla casualità dei numeri generati in Python: https://realpython.com/python-random/