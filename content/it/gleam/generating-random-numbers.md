---
title:    "Gleam: Generazione di numeri casuali"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/gleam/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Perché
Generare numeri casuali è una funzionalità importante per molti programmi. Potresti voler simulare processi casuali o creare giochi con elementi casuali. In ogni caso, la generazione di numeri casuali è uno strumento fondamentale per diversi scenari di programmazione.

## Come Fare
Per generare numeri casuali in Gleam, puoi utilizzare la libreria standard `random`. Per prima cosa, dovrai importare la libreria all'inizio del tuo file di codice:

```
use random
```

Quindi, puoi utilizzare la funzione `int` per generare un numero intero casuale all'interno di un determinato intervallo. Ad esempio, se volessi generare un numero tra 1 e 10, puoi usare il seguente codice:

```
let random_number = random.int(1, 10)
```

Puoi anche utilizzare la funzione `float` per generare un numero decimale casuale. Ad esempio, se volessi generare un numero tra 0 e 1, puoi usare il seguente codice:

```
let random_float = random.float(0.0, 1.0)
```

Infine, se vuoi generare una sequenza di numeri casuali, puoi utilizzare la funzione `generate` fornendo l'intervallo e il numero desiderato di numeri da generare. Ad esempio, se volessi generare una lista di 5 numeri casuali compresi tra 1 e 100, puoi utilizzare il seguente codice:

```
let random_list = random.generate(1, 100, 5)
```

## Approfondimento
La generazione di numeri casuali è una tecnica fondamentale in programmazione, ma va utilizzata con cautela. Assicurati sempre di capire le proprietà dei numeri casuali generati e come potrebbero influenzare il tuo programma. Inoltre, ci sono altri modi per generare numeri casuali in Gleam, come utilizzando una seme di generazione specifica oppure utilizzando la libreria `crypto` per generare numeri crittograficamente sicuri.

## Vedi Anche
- [Documentazione ufficiale sulla libreria random di Gleam](https://gleam.run/libraries/random)
- [Esempi di codice per la generazione di numeri casuali in Gleam](https://github.com/search?l=Gleam&q=random&type=Code)