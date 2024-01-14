---
title:    "Haskell: Calcolo di una data nel futuro o nel passato"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Perché
Calcolare una data nel futuro o nel passato può essere utile per una varietà di motivi, come programmare eventi futuri o tenere traccia di scadenze passate.

## Come fare
Per calcolare una data nel futuro o nel passato utilizzando Haskell, è possibile utilizzare la funzione `addDays` della libreria `Data.Time`. Di seguito è riportato un esempio di codice che calcola la data di domani:

```Haskell
import Data.Time

main = do
  let today = utctDay getCurrentTime
      tomorrow = addDays 1 today
  putStrLn $ show tomorrow -- output: 2021-08-04
```

## Approfondimento
La funzione `addDays` prende due argomenti: il numero di giorni da aggiungere e la data di partenza. È importante notare che la funzione non modifica la data di partenza, ma restituisce una nuova data. È inoltre possibile utilizzare numeri negativi per calcolare una data nel passato. Altre funzioni utili della libreria `Data.Time` includono `diffDays` per calcolare la differenza in giorni tra due date e `getCurrentTime` per ottenere l'ora e la data correnti.

## Vedi anche
- [Documentazione ufficiale di Data.Time](https://hackage.haskell.org/package/time/docs/Data-Time.html)
- [Tutorial su come calcolare date nel futuro o nel passato in Haskell](https://www.geeksforgeeks.org/calculating-future-and-past-date-in-haskell/)
- [Esempi di codice per operazioni su date in Haskell](https://wiki.haskell.org/Date_arithmetic)