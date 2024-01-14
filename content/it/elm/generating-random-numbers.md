---
title:    "Elm: Generazione di numeri casuali"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/elm/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Perché

Perché qualcuno dovrebbe impegnarsi nella generazione di numeri casuali? I numeri casuali sono essenziali per un ampio spettro di applicazioni, come giochi di carte, giochi di sorteggio e simulazioni scientifiche. Con Elm, possiamo generare numeri casuali in modo affidabile e mantenere la nostra applicazione funzionale e stabile.

## Come Fare

Per generare numeri casuali in Elm, possiamo utilizzare il modulo `Random` integrato nella libreria di base. Iniziamo dichiarando il nostro generatore di numeri casuali utilizzando la funzione `Random.initialSeed`:

```
Elm
-- dichiarazione del generatore di numeri casuali
let
  generator = Random.initialSeed 12345
```

Ora possiamo utilizzare il nostro generatore per generare un numero casuale utilizzando la funzione `Random.int`:

```
Elm
-- generazione di un numero casuale tra 1 e 10
let
  number = generator
    |> Random.step (Random.int 1 10)
    |> Random.generate MyMsg
```

Dobbiamo anche assicurarci di gestire il risultato della nostra generazione di numeri casuali nel nostro programma, come mostrato sopra mediante la funzione `Random.generate`. 

## Approfondimento

Per comprendere meglio come funziona la generazione di numeri casuali in Elm, possiamo esaminare come la libreria di base gestisce i generatori di numeri casuali. I generatori di numeri casuali sono fondamentalmente funzioni che prendono un seme iniziale e restituiscono una coppia di valori: il prossimo seme e il numero casuale generato. Utilizzando la funzione `Random.step`, possiamo passare il generatore di numeri casuali attraverso una serie di passaggi per ottenere il prossimo seme e il numero casuale. In questo modo, possiamo garantire che ogni numero casuale generato sia davvero casuale e non dipenda da valori esterni.

## Vedi Anche

* La documentazione ufficiale di Elm su `Random`: https://package.elm-lang.org/packages/elm/random/latest/Random
* Un esempio pratico di generazione di numeri casuali in Elm: https://medium.com/@ckoster22/producing-random-output-in-elm-19bfab9c2fa4
* Altri articoli sulla programmazione funzionale in Elm: https://www.jessesquires.com/tags/functional-programming/