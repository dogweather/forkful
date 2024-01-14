---
title:                "Gleam: Generazione di numeri casuali"
simple_title:         "Generazione di numeri casuali"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Perché

Generare numeri casuali è un'attività comune nella programmazione, utilizzata per scopi come la simulazione di eventi casuali o per generare dati casuali per scopi di testing. In Gleam, è possibile generare numeri casuali utilizzando il modulo `Random`.

## Come

Per generare un numero casuale intero in Gleam, possiamo utilizzare la funzione `int` del modulo `Random`. Ad esempio, per generare un numero casuale compreso tra 1 e 100, possiamo utilizzare il seguente codice:

```Gleam
import Random

let num = Random.int(1, 100)
```

Questo restituirà un numero casuale compreso tra 1 e 100.

Per generare un numero casuale di tipo `Float` invece, possiamo utilizzare la funzione `float` del modulo `Random`. Ad esempio, per generare un numero casuale compreso tra 0 e 1, possiamo utilizzare il seguente codice:

```Gleam
import Random

let num = Random.float(0.0, 1.0)
```

## Deep Dive

In Gleam, il modulo `Random` si basa sull'algoritmo di randomizzazione `xorshift`. Questo algoritmo è noto per essere veloce ed efficiente, e garantisce una buona distribuzione dei numeri casuali.

Tuttavia, è importante notare che i numeri generati dal modulo `Random` non sono veramente casuali, ma sono solo pseudo-casuali. Ciò significa che se utilizziamo gli stessi parametri per generare numeri casuali, otterremo sempre lo stesso risultato. Questo può essere utile per scopi di testing, ma non è consigliato per scopi di sicurezza critici.

## Vedi anche

- La documentazione ufficiale del modulo `Random`: https://gleam.run/modules/random
- Un articolo sulle basi della generazione di numeri casuali in programmazione: https://www.geeksforgeeks.org/generating-random-numbers-in-java/