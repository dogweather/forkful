---
title:    "Haskell: Ottenere la data corrente"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Perché

Se sei un programmatore Haskell, potresti essere curioso di sapere come ottenere la data corrente nel tuo codice. La data è un'informazione importante che viene spesso utilizzata in applicazioni e software. Inoltre, imparare a gestire le date correttamente può aiutarti a scrivere codice più robusto e affidabile.

## Come fare

Una delle prime cose da fare per ottenere la data corrente in Haskell è importare il modulo ```Data.Time```. Questo modulo contiene diverse funzioni per lavorare con date e orari.

```
import Data.Time
```

Una delle funzioni più utilizzate è ```getCurrentTime``` che restituisce un oggetto ```UTCTime``` contenente l'ora e la data corrente. Per utilizzare questa funzione, è necessario utilizzare la monade ```IO``` e l'operatore ```/``` per dividere l'oggetto ```UTCTime``` in singole componenti come giorno, mese, anno, ora, minuti e secondi.

```
main :: IO ()
main = do
  now <- getCurrentTime
  let (year, month, day) = toGregorian $ utctDay now
      (hour, minute, second) = timeToTimeOfDay $ utctDayTime now
  putStrLn $ "La data corrente è " ++ show day ++ "/" ++ show month ++ "/" ++ show year ++
              " e l'ora è " ++ show hour ++ ":" ++ show minute ++ ":" ++ show second
```

L'esempio di codice sopra restituirà qualcosa del genere: ```La data corrente è 9/7/2021 e l'ora è 14:30:25```.

## Approfondimento

Oltre alla funzione ```getCurrentTime```, il modulo ```Data.Time``` offre molte altre opzioni per gestire le date e gli orari. Ad esempio, puoi convertire una data in diversi formati, aggiungere o sottrarre un periodo di tempo da una data, o calcolare la differenza tra due date.

Inoltre, se stai lavorando con fusi orari, il modulo ```Data.Time.LocalTime``` può essere utile. Questo modulo aggiunge funzioni per gestire le date e gli orari in modo locale, tenendo conto dei fusi orari.

## Vedi anche

- [Documentazione del modulo Data.Time](https://hackage.haskell.org/package/time/docs/Data-Time.html)
- [Documentazione del modulo Data.Time.LocalTime](https://hackage.haskell.org/package/time/docs/Data-Time-LocalTime.html)
- [Tutorial sulle date e gli orari in Haskell](https://www.schoolofhaskell.com/user/marcina/date-and-time)