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

## Cos'è e perché?

Generare numeri casuali è un processo fondamentale nella programmazione. Ciò consente ai programmatori di creare elementi casuali nei loro programmi, come generare password sicure o selezionare un elemento a caso in una lista.

## Come fare:

Per generare un numero casuale in Gleam, puoi utilizzare la funzione `random.int` fornita dalla libreria standard `gleam/random`. Esempio:

```Gleam
import gleam/random.{int}

let num = random.int(1, 100) // genera un numero casuale tra 1 e 100
```

Output: il valore di `num` sarà un numero casuale compreso tra 1 e 100.

Puoi anche utilizzare la funzione `random.float` per generare numeri casuali con la virgola. Esempio:

```Gleam
import gleam/random.{float}

let num = random.float(0, 1) // genera un numero float tra 0 e 1
```

Output: il valore di `num` sarà un numero casuale con la virgola compreso tra 0 e 1.

## Approfondimento:

La generazione di numeri casuali è stata storicamente una sfida per i programmatori, con diverse tecniche sviluppate nel corso degli anni per rendere i numeri più "casuali". In Gleam, vengono utilizzati algoritmi di generazione di numeri pseudo-casuali, che utilizzano un particolare metodo matematico per produrre una sequenza di numeri che appaiono casuali.

Un'alternativa a questi algoritmi è l'utilizzo di generatori di numeri veramente casuali, come ad esempio attraverso una fonte esterna, come un dispositivo fisico che registra l'entropia ambientale.

Per chi vuole approfondire l'implementazione dei generatori di numeri casuali in Gleam, consigliamo di consultare il codice sorgente della libreria `gleam/random` e di approfondire gli algoritmi di generazione di numeri pseudo-casuali.

## Vedi anche:

- Documentazione ufficiale sulla libreria `gleam/random`: https://gleam.run/modules/gleam/random/latest/
- Articolo su algoritmi di generazione di numeri pseudo-casuali: https://en.wikipedia.org/wiki/Pseudorandom_number_generator