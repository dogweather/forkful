---
title:                "Generazione di numeri casuali"
html_title:           "Gleam: Generazione di numeri casuali"
simple_title:         "Generazione di numeri casuali"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Perché
Puoi pensare che generare numeri casuali sia solo per giochi o attività divertenti, ma in realtà è un'abilità utile da avere per molte applicazioni pratiche. Ad esempio, può essere utilizzato per testare algoritmi o per creare dati di esempio per il tuo programma.

## Come fare
Per generare numeri casuali in Gleam, puoi utilizzare la funzione `Random.int` che accetta un numero minimo e massimo e restituisce un intero casuale compreso tra questi due valori inclusi.

```Gleam
import Random

let random_num = Random.int(1, 10)

io.println(random_num)
```

Questo codice genererà e stamperà un numero casuale compreso tra 1 e 10 ogni volta che viene eseguito.

Puoi anche utilizzare la funzione `Random.float` per generare un numero casuale con la virgola.

```Gleam
import Random

let random_num = Random.float(0.0, 1.0)

io.println(random_num)
```

In questo caso, il numero casuale sarà compreso tra 0.0 e 1.0.

## Approfondimento
La generazione di numeri casuali in un programma richiede un algoritmo che sia in grado di riprodurre una serie di numeri che sembrano casuali. Uno dei metodi più comuni per farlo è il "linear congruential generator", che utilizza una formula matematica per generare una sequenza di numeri con proprietà statistiche simili a quelle di una sequenza casuale.

Se vuoi esplorare ulteriormente il funzionamento di questo algoritmo e altre strategie per la generazione di numeri casuali, puoi consultare questi link:

- [Linear congruential generator - Wikipedia](https://en.wikipedia.org/wiki/Linear_congruential_generator)
- [How to generate random numbers · Gleam](https://gleam.run/book/tour/random.html)

## Vedi anche
- [Gleam Documentation](https://gleam.run/) - Documentazione ufficiale di Gleam.
- [Gleam on GitHub](https://github.com/gleam-lang/gleam) - Repository GitHub di Gleam.