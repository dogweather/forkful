---
title:                "Calcolare una data nel futuro o nel passato"
html_title:           "Haskell: Calcolare una data nel futuro o nel passato"
simple_title:         "Calcolare una data nel futuro o nel passato"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/haskell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Calcolo delle Date Futuri e Passate in Haskell

## Cosa & Perché?
Calcolare una data futura o passata significa aggiungere o sottrarre un certo numero di giorni ad una data specifica. Questo è di solito utilizzato dai programmatori per fare previsioni, per tracciare l'andamento dei dati nel tempo, o per gestire eventi programmati.

## Come Fai:
Haskell offre il pacchetto `Data.Time`, che semplifica il calcolo delle date. 

Possiamo calcolare una data futura aggiungendo giorni a una data specifica, come segue:

```Haskell
import Data.Time

main = do
    let oggi = fromGregorian 2022 1 1
    print $ addDays 30 oggi  -- Stampa "2022-01-31"
```

Per una data passata, sottraiamo giorni:

```Haskell
import Data.Time

main = do
    let oggi = fromGregorian 2022 1 1
    print $ addDays (-30) oggi  -- Stampa "2021-12-02"
```

## Approfondimento
Storicamente, il calcolo delle date è stato un problema noto nella programmazione. Gli scienziati della computer della metà del ventesimo secolo dovevano risolvere molteplici problemi, come le date di inizio e fine dell'anno bisestile e le differenze tra vari calendari.

Alternativamente, è anche possibile utilizzare pacchetti Haskell come time-lens o date-cache per calcoli più avanzati. Il pacchetto `Data.Time` è generalmente facile da usare per il calcolo di date più semplici. 

L'implementazione di `addDays` in Haskell sfrutta l'arithmetica con i numeri interi. La funzione `addDays` aggiunge il numero specificato di giorni alla data fornita, sfruttando il fatto che i giorni sono rappresentati come numeri interi nel sistema Gregoriano.

## Riferimenti Utili
1. Documentazione `Data.Time`: http://hackage.haskell.org/package/time-1.9.3/docs/Data-Time.html
2. Haskell Wiki Data Durations: https://wiki.haskell.org/Data_duration
3. Libreria time-lens: http://hackage.haskell.org/package/time-lens