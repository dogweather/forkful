---
title:                "Haskell: Ottenere la data corrente"
programming_language: "Haskell"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/haskell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Perché

La data corrente è un'informazione fondamentale in quasi tutti i programmi che scriviamo. Sapere come ottenerla è essenziale per gestire correttamente i dati temporalmente correlati o semplicemente per visualizzarli nei nostri programmi.

## Come ottenerla

Per ottenere la data corrente in Haskell, dobbiamo utilizzare il modulo `Data.Time` dalla libreria `time`. Possiamo farlo in diversi modi, ma il modo più semplice è utilizzando la funzione `getCurrentTime`.

```Haskell
import Data.Time

main = do
    time <- getCurrentTime
    putStrLn $ "La data e l'ora correnti sono: " ++ show time
```

L'output sarà qualcosa del genere:

```
La data e l'ora correnti sono: 2021-03-31 10:24:18.487682 UTC
```

In questo esempio, stiamo semplicemente ottenendo la data e l'ora correnti e stampandole a schermo utilizzando la funzione `putStrLn` e l'operatore di concatenazione `++`.

Ma cosa succede se vogliamo ottenere la data in un formato diverso, come ad esempio "DD/MM/YYYY"? In questo caso, dobbiamo utilizzare la funzione `formatTime`:

```Haskell
import Data.Time
import System.Locale

main = do
    time <- getCurrentTime
    let formattedTime = formatTime defaultTimeLocale "%d/%m/%Y" time
    putStrLn $ "La data corrente è: " ++ formattedTime
```

Ecco l'output che riceveremo:

```
La data corrente è: 31/03/2021
```

## Approfondimento

Come accennato in precedenza, la funzione `getCurrentTime` restituisce l'ora corrente nel formato UTC. Ma cosa significa UTC? UTC (Coordinated Universal Time) è il tempo universale coordinato, una scala di tempo di riferimento utilizzata in molti paesi. È equivalente al tempo di Greenwich e viene spesso utilizzato come base per la gestione del tempo in tutto il mondo.

Per ottenere la data e l'ora corrente nel fuso orario locale, possiamo utilizzare la funzione `getZonedTime`:

```Haskell
import Data.Time.LocalTime

main = do
    time <- getZonedTime
    putStrLn $ "La data e l'ora correnti nel fuso orario locale sono: " ++ show time
```

L'output sarà simile a questo:

```
La data e l'ora correnti nel fuso orario locale sono: 2021-03-31 12:24:18.487682 CET
```

Esistono molte altre funzioni e tipi di dati definiti nel modulo `Data.Time` per lavorare con il tempo e la data in Haskell. Ti consiglio di dare un'occhiata alla documentazione ufficiale per ulteriori informazioni.

## Vedere anche

- [Documentazione ufficiale di `time`](https://hackage.haskell.org/package/time/docs/Data-Time.html)
- [Tutorial su come lavorare con il tempo in Haskell](https://mmhaskell.com/blog/2017/4/13/working-with-time)
- [Funzioni di formattazione avanzata per il tempo in Haskell](https://mmhaskell.com/lst/working-with-time/formatting-time)