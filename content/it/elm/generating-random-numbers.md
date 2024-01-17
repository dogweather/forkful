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

## Cosa & Perché?
Generare numeri casuali è una tecnica comune utilizzata dai programmatori per creare una fonte di dati aleatoria nei loro programmi. Questo può essere utile in situazioni come i giochi, dove la casualità è una parte importante dell'esperienza.

## Come fare:
Di seguito ci sono due esempi di codice per generare numeri casuali in Elm. Nella prima, useremo la funzione `Random.int` per generare un intero casuale compreso tra 1 e 10. Nella seconda, utilizzeremo `Random.float` per ottenere un numero a virgola mobile casuale tra 0 e 1. Entrambi gli esempi utilizzano un generatore random predefinito.

```
import Random exposing (int, float)

myRandomInt = 
    Random.int 1 10 

myRandomFloat = 
    Random.float 0 1 
```

L'output sarà simile a questo:

```
myRandomInt =>
6

myRandomFloat =>
0.24879926704549196
```

## Approfondimento:
La generazione di numeri casuali è diventata una pratica comune nelle applicazioni informatiche fin dagli anni '60. In passato, i programmatori utilizzavano spesso algoritmi matematici per generare numeri "pseudo-casuali", poiché i computer non avevano la capacità di generare numeri veramente casuali. Tuttavia, con l'avanzamento della tecnologia, ora esistono anche generatori di numeri casuali fisici che utilizzano fenomeni naturali, come il rumore ambientale, per produrre numeri veramente casuali.

In Elm, è anche possibile utilizzare la libreria `elm-random-extra` per ottenere una maggiore flessibilità nella generazione di numeri casuali, ad esempio specificando un seed o selezionando un tipo di distribuzione.

## Vedi anche:
- La documentazione ufficiale di Elm sulla generazione di numeri casuali: https://package.elm-lang.org/packages/elm/random/latest/
- La libreria `elm-random-extra`: https://package.elm-lang.org/packages/elm-community/random-extra/latest/