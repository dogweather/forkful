---
title:                "Elm: Calcolare una data nel futuro o nel passato."
simple_title:         "Calcolare una data nel futuro o nel passato."
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elm/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Perché

Ci sono molte situazioni in cui potresti voler calcolare una data specifica in futuro o in passato. Ad esempio, potresti avere un programma di prenotazione che chiede all'utente di selezionare una data di partenza o di arrivo. O forse stai creando un calendario con un'opzione per spostare tra i mesi. In ogni caso, è importante comprendere come calcolare correttamente una data futura o passata in Elm.

## Come fare

Calcolare una data in Elm è abbastanza semplice e si può fare utilizzando alcune funzioni standard della libreria Time di Elm. Per calcolare una data futura o passata, è necessario utilizzare la funzione `add` che accetta tre argomenti: un intero che rappresenta l'ammontare di tempo da aggiungere o sottrarre, un intervallo di tempo (such as `millisecond`, `second`, `minute`, etc.), e una data base di riferimento. Ecco un esempio di codice che calcola la data di oggi più 5 giorni:

```Elm
import Time exposing (..)

add 5 day (today)
```

L'output del codice sarà il seguente:

```Elm
Teak Time exposing (..)
"2019-10-11"
```

Oltre alle funzioni `add` e `today`, la libreria Time di Elm offre molte altre funzioni utili per il calcolo di date e orari. È possibile esplorarle ulteriormente nella documentazione ufficiale di Elm.

## Approfondimento

Per calcolare una data futura o passata in modo più preciso e complesso, potresti voler prendere in considerazione anche i leap years, i fusi orari e i giorni festivi. In questo caso, potresti dover utilizzare una libreria esterna o scrivere il tuo codice personalizzato. Inoltre, ricorda che le date in Elm sono immutabili, il che significa che una volta create, non possono essere modificate. Sarà necessario quindi utilizzare le funzioni di creazione e manipolazione delle date per ottenere risultati più accurati.

## Vedi anche

- [Documentazione ufficiale di Elm su Date e Time](https://package.elm-lang.org/packages/elm/time/latest/)
- [Esempi di calcolo di date in Elm](https://www.itabari.com/elm/it/articles/visually-review-date-time-in-Elm.html)
- [Il codice sorgente del progetto Elm](https://github.com/elm/compiler/blob/master/hints/date.md)