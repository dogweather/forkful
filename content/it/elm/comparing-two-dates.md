---
title:                "Elm: Confrontare due date"
simple_title:         "Confrontare due date"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elm/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Perché
Quando si lavora con date in un'applicazione Elm, può essere utile confrontare due date per vedere quale delle due sia precedente o successiva. Imparare come fare questo può rendere il tuo codice più efficiente e preciso.

## Come Fare
Per confrontare due date in Elm, è necessario prima convertire le date in formato Time e poi utilizzare la funzione `compare` per determinare la loro relazione. Di seguito è riportato un esempio di codice che mostra come fare ciò:

```Elm
import Time
...
date1 : Date
date2 : Date

-- Converte le date in Timestamp
timestamp1 = Time.toPosix date1
timestamp2 = Time.toPosix date2

-- Confronta i timestamp utilizzando la funzione compare
comparison = Time.compare timestamp1 timestamp2

-- Output: LT (menor que), GT (mayor que), o EQ (igual)
```

## Approfondimento
La funzione `compare` è molto utile per confrontare due date, ma è importante notare che può essere applicata anche ad altri tipi di dati come numeri, stringhe o booleani. Inoltre, ci sono altre funzioni utili per lavorare con date in Elm, come `add`, `sub`, `round` ed `hoursSince` che ti permettono di fare calcoli e modifiche alle date in modo semplice e intuitivo.

## Vedi Anche
- [Documentazione ufficiale di Elm sulle Date](https://package.elm-lang.org/packages/elm/time/latest/Time)
- [Tutorial su come manipolare le date in Elm](https://eriktim.github.io/how-to/2018/07/17/dates-in-elm.html)
- [Esempi di applicazioni Elm che utilizzano date](https://github.com/sporto/elm-datepicker)