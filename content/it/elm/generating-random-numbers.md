---
title:                "Generazione di numeri casuali"
html_title:           "Elm: Generazione di numeri casuali"
simple_title:         "Generazione di numeri casuali"
programming_language: "Elm"
category:             "Elm"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elm/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Perché

Se stai cercando di aggiungere un po' di casualità e variabilità ai tuoi programmi in Elm, allora la generazione di numeri casuali potrebbe essere ciò che stai cercando! Con la funzione di generazione di numeri casuali nativa di Elm, puoi creare diversi scenari e interazioni divertenti all'interno delle tue applicazioni.

## Come procedere 

Per generare numeri casuali in Elm, dovrai utilizzare la funzione `Random.generate` e fornire un generatore per il tipo di numero che desideri ottenere. Ad esempio, se vuoi generare un numero intero compreso tra 1 e 10, puoi scrivere:

```elm
Random.generate Random.int (1, 10)
```

Il risultato sarà un dato di tipo `Cmd msg`, quindi dovrai gestirlo nella tua architettura di Elm. Puoi farlo utilizzando la funzione `Cmd.map` per mappare il tuo dato generato a un `msg` che può essere gestito nella tua funzione `update`.

## Approfondimento

La funzione `Random.generate` utilizza un generatore, che è essenzialmente una funzione che accetta uno stato iniziale e restituisce una tupla contenente il valore generato e lo stato aggiornato. Puoi trovare diversi generatori predefiniti nella libreria standard di Elm, ma puoi anche creare i tuoi generatori personalizzati utilizzando la funzione `Random.custom`. Questo può essere utile se hai esigenze precise nella generazione di numeri casuali per il tuo programma.

## Vedi anche

- Documentazione ufficiale di Elm sulla generazione di numeri casuali: https://elm-lang.org/docs/random 
- Esempi di generazione di numeri casuali in Elm: https://ellie-app.com/6CHXKqZRydQa1 
- Tutorial su come gestire la funzione `Random` in Elm: https://www.youtube.com/watch?v=qvSamvFY7yA