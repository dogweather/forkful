---
title:                "Gleam: Generazione di numeri casuali"
programming_language: "Gleam"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Perché

Generare numeri casuali è essenziale per molti algoritmi e applicazioni, come giochi, crittografia e simulazioni. È importante avere una conoscenza di base di come generare numeri casuali per comprendere meglio il funzionamento di queste applicazioni.

## Come fare

Per generare numeri casuali in Gleam, possiamo utilizzare il modulo `random` che offre una varietà di funzioni per generare numeri casuali in diversi formati.

```Gleam
import random

// Genera un numero intero casuale compreso tra 0 e 10 inclusi
let random_int = random.int_uniform(0, 10)
```

Possiamo anche utilizzare la funzione `float_uniform` per generare numeri casuali in formato floating point. Inoltre, possiamo specificare la semina (seed) per ottenere sempre lo stesso numero casuale in caso di dover riprodurre il nostro codice.

```Gleam
// Genera un numero floating point casuale compreso tra 0 e 1 inclusi
let random_float = random.float_uniform(0.0, 1.0, seed=1234)
```

Ci sono anche diverse funzioni nel modulo `random` per generare numeri casuali con diversi distribuzioni, come la distribuzione normale o la distribuzione esponenziale.

## Approfondimento

Nell'implementazione di Gleam, i numeri casuali sono generati utilizzando l'algoritmo Marsaglia, che è stato dimostrato essere un buon generatore di numeri casuali. Inoltre, è possibile utilizzare il modulo `rand` che offre più opzioni per la semina, inclusa la possibilità di utilizzare un generatore di numeri casuali esterno.

# Vedi anche

- Documentazione ufficiale di Gleam sul modulo `random`: https://gleam.run/modules/random/
- Tutorial su come generare numeri casuali in Gleam: https://gleam.run/blog/generating-random-numbers-in-gleam/
- Esempi di applicazioni in cui sono necessari numeri casuali: https://medium.com/@asminasarvin/random-numbers-importance-of-randomness-in-everyday-life-42205e5f181f