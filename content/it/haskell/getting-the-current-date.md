---
title:                "Ottenere la data corrente"
date:                  2024-01-20T15:15:06.127514-07:00
html_title:           "Arduino: Ottenere la data corrente"
simple_title:         "Ottenere la data corrente"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/haskell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why?
Ottenere la data corrente in Haskell è fondamentale per interagire con il tempo reale nei nostri programmi, da semplici log a funzionalità basate sulla data.

## How to:
Per ottenere la data corrente in Haskell, utilizza il modulo `Data.Time`. Prima, assicurati che la libreria `time` sia elencata nel tuo file cabal o stack.

```Haskell
import Data.Time

main :: IO ()
main = do
  currentDate <- getCurrentTime
  print $ utctDay currentDate
```

Eseguendo il programma riceverai un output simile al seguente:

```
2023-04-05
```

## Deep Dive
Haskell non include funzioni per il tempo nel suo Prelude, quindi usiamo `Data.Time`, una libreria standard disponibile da `ghc-6.6.1`. L'alternativa meno diretta sarebbe utilizzare la libreria `old-time`, ma è più complicata e, beh, vecchia. `Data.Time` è preferibile perché segue gli standard internazionali (ISO 8601) e offre una maggiore flessibilità.

Una specifica attenzione va al trattamento dei fusi orari. Per default, `getCurrentTime` ti restituisce l'UTC. Se necessiti del fuso orario locale, entra in gioco `getCurrentTimeZone`. Ecco come:

```Haskell
import Data.Time

main :: IO ()
main = do
  timeZone <- getCurrentTimeZone
  zonedTime <- getZonedTime
  print zonedTime
  let localTime = utcToLocalTime timeZone $ zonedTimeToUTC zonedTime
  putStrLn $ "Data locale: " ++ show (localDay localTime)
```

Dovresti vedere un output che rispecchia il tuo fuso orario locale:

```
Data locale: 2023-04-05
```

Questo riporta l'attenzione sui dettagli di implementazione, come la rappresentazione del tempo in Haskell tramite la struttura `UTCTime`. Questo fa sì che le operazioni sul tempo siano coerenti a livello globale, evitando confusione su diverse rappresentazioni.

## See Also
Per una panoramica più completa delle funzionalità relative alla data e ora in Haskell, consulta le seguenti risorse:

- Data.Time library on Hackage: [https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time.html](https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time.html)
- Layering Time: [http://chrisdone.com/posts/measuring-duration-in-haskell](http://chrisdone.com/posts/measuring-duration-in-haskell)
