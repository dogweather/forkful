---
date: 2024-01-20 17:36:48.723872-07:00
description: "Convertire una data in una stringa significa trasformare la rappresentazione\
  \ di un momento temporale in testo leggibile. Lo facciamo per memorizzare,\u2026"
lastmod: '2024-03-13T22:44:43.486528-06:00'
model: gpt-4-1106-preview
summary: Convertire una data in una stringa significa trasformare la rappresentazione
  di un momento temporale in testo leggibile.
title: Conversione di una data in una stringa
weight: 28
---

## What & Why? (Cosa & Perché?)
Convertire una data in una stringa significa trasformare la rappresentazione di un momento temporale in testo leggibile. Lo facciamo per memorizzare, visualizzare, e condividere date in un formato comprensibile agli esseri umani.

## How to: (Come fare:)
```Haskell
import Data.Time

-- Assume che abbiamo il current time (ora corrente)
main :: IO ()
main = do
    currentTime <- getCurrentTime
    let dateString = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" currentTime
    putStrLn dateString
```
Output:
```
2023-03-04 12:30:45
```

## Deep Dive (Approfondimento)
Haskell gestisce date e tempo tramite il modulo `Data.Time`. Questo è diventato lo standard dopo che la community ha riconosciuto il bisogno di un sistema di gestione del tempo robusto e versatile. Esistono alternative come `old-time` ma sono in gran parte obsolete. `formatTime` si basa su `TimeLocale`, che permette di formattare una data in modi specifici per ogni cultura. La flessibilità è un ingrediente chiave here: possiamo usare formati predefiniti o creare i nostri.

## See Also (Vedi Anche)
- [Haskell Time Library](https://hackage.haskell.org/package/time): Documentazione ufficiale del modulo `Data.Time`.
- [LYAHFGG: Dates and Times](http://learnyouahaskell.com/input-and-output#dates-and-times): Sezione di "Learn You a Haskell for Great Good!" su date e tempo.
