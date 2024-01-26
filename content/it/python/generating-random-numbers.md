---
title:                "Generazione di numeri casuali"
date:                  2024-01-20T17:49:36.419911-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generazione di numeri casuali"
programming_language: "Python"
category:             "Python"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/python/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why?
Generare numeri casuali in Python significa creare valori imprevedibili. I programmatori lo fanno per simulazioni, giochi, test e sicurezza informatica.

## How to:
Per iniziare, usa il modulo `random`. Ecco degli esempi:

```Python
import random

# Genera un numero casuale tra 1 e 10
numero = random.randint(1, 10)
print(numero)

# Genera un numero in virgola mobile tra 0 e 1
fluttuante = random.random()
print(fluttuante)
```

Sample output potrebbe essere:

```
7
0.320389112345
```

## Deep Dive
Generare numeri casuali non è una novità. Ai tempi dei primi computer, si usavano metodi meccanici e statistici. Oggi, i computer usano algoritmi detti generatori di numeri pseudo-casuali, perché sono deterministici ma sembrano casuali.

Un'alternativa è l'uso di generatori di numeri casuali veri, basati su fenomeni fisici. Python non li implementa di default, ma possono essere integrati tramite estensioni o hardware esterno.

Nei dettagli, `random.randint` usa l'algoritmo Mersenne Twister per generare un numero. È veloce e sufficientemente "casuale" per gli usi comuni, ma non per tutti gli ambiti di sicurezza.

## See Also
Per approfondimenti e avanzamenti:

- Documentazione ufficiale di Python: [random — Generate pseudo-random numbers](https://docs.python.org/3/library/random.html)
- Per generatori crittograficamente sicuri: [secrets — Generate secure random numbers for managing secrets](https://docs.python.org/3/library/secrets.html)
