---
title:    "Gleam: Generazione di numeri casuali"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Perché

Molti programmatori devono generare numeri casuali per vari scopi, come la creazione di giochi o l'analisi statistica. Con il linguaggio di programmazione Gleam, è possibile generare in modo semplice e affidabile numeri casuali per soddisfare le proprie esigenze.

## Come Fare

Per generare un numero casuale in Gleam, utilizziamo il modulo `Random` e la funzione `int`. Ad esempio, se vogliamo generare un numero casuale compreso tra 1 e 10, possiamo scrivere il seguente codice:

```Gleam
import random
let num = random.int(1, 10)
```

Ecco un esempio di output:

```Gleam
6
```

Possiamo anche generare più di un numero casuale alla volta utilizzando la funzione `list` del modulo `Random`. Ad esempio, se vogliamo generare una lista di 5 numeri casuali tra 1 e 100, possiamo scrivere il seguente codice:

```Gleam
import random
let nums = random.list(5, 1, 100)
```

Ecco un esempio di output:

```Gleam
[73, 42, 11, 86, 99]
```

## Approfondimento

Il modulo `Random` di Gleam utilizza l'algoritmo di generazione di numeri casuali Mersenne Twister, che è noto per la sua robustezza e affidabilità. Inoltre, possiamo specificare un seed per controllare la sequenza di numeri generata, rendendo il processo riproducibile.

È importante notare che la generazione di numeri casuali non deve mai essere utilizzata per scopi di sicurezza, poiché l'algoritmo utilizzato non è stato progettato per questo. Invece, è consigliato utilizzarlo per scopi di simulazione o per divertimento.

## Vedi Anche

- Documentazione ufficiale del modulo Random di Gleam: https://gleam.run/modules/random
- Esempi di utilizzo di numeri casuali in Gleam: https://github.com/gleam-lang/gleam/blob/master/lib/random/examples/
- Thread su Stack Overflow con diverse implementazioni di generazione di numeri casuali in Gleam: https://stackoverflow.com/questions/54782085/generating-random-numbers-in-gleam