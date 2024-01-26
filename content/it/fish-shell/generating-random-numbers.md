---
title:                "Generazione di numeri casuali"
date:                  2024-01-20T17:49:02.770337-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generazione di numeri casuali"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/fish-shell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Che cosa & Perché?
Generare numeri casuali è l'atto di creare un valore imprevedibile e non ripetitivo. I programmatori lo fanno per casi d'uso come test, simulazioni ed elementi di gioco.

## Come fare:
Esempi di codice e output di esempio sono mostrati qui sotto.

```Fish
# Genera un numero casuale tra 0 e 999.
set numero (random 0 999)
echo $numero
```

Output potrebbe essere:
```
657
```

```Fish
# Genera un numero casuale ed assegnalo ad una variabile.
set mislancio (random)
echo "Il lancio del dado ha dato: "$mislancio
```

Output potrebbe essere:
```
Il lancio del dado ha dato: 21023
```

## Approfondimento
I numeri casuali sono fondamentali in informatica fin dai primi computer. La Fish Shell utilizza una funzione built-in chiamata `random` per generare numeri casuali. In altri linguaggi, come Python o JavaScript, ci sono funzioni come `random` o `Math.random()`. 

I numeri casuali in un computer non sono veramente casuali; sono detti pseudo-casuali perché generati algoritmicamente. La randomizzazione di Fish si basa sul generatore di numeri pseudo-casuali di libc e migliora la casualità con valori come l'orario attuale.

## Vedi Anche
- Documentazione ufficiale Fish: [https://fishshell.com/docs/current/commands.html#random](https://fishshell.com/docs/current/commands.html#random)
- Funzione random POSIX per confronto: [https://pubs.opengroup.org/onlinepubs/9699919799/functions/rand.html](https://pubs.opengroup.org/onlinepubs/9699919799/functions/rand.html)
- Discussione sulla casualità in informatica: [https://it.wikipedia.org/wiki/Generatore_di_numeri_pseudo-casuali](https://it.wikipedia.org/wiki/Generatore_di_numeri_pseudo-casuali)
