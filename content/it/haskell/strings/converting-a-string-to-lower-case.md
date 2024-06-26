---
date: 2024-01-20 17:38:41.355565-07:00
description: 'How to: In Haskell, possiamo utilizzare `Data.Text.toLower` per convertire
  una stringa in minuscolo. Ecco un esempio.'
lastmod: '2024-03-13T22:44:43.462584-06:00'
model: gpt-4-1106-preview
summary: In Haskell, possiamo utilizzare `Data.Text.toLower` per convertire una stringa
  in minuscolo.
title: Conversione di una stringa in minuscolo
weight: 4
---

## How to:
In Haskell, possiamo utilizzare `Data.Text.toLower` per convertire una stringa in minuscolo. Ecco un esempio:

```haskell
import Data.Text (toLower, pack, unpack)

toLowerCase :: String -> String
toLowerCase = unpack . toLower . pack

main :: IO ()
main = putStrLn $ toLowerCase "Ciao Mondo!"
```

Output:
```
ciao mondo!
```

## Deep Dive
La funzione `toLower` di `Data.Text` è più efficiente delle soluzioni basate su liste, grazie all'ottimizzazione delle operazioni su testi di `Data.Text`. Prima di `Data.Text`, i programmatori di Haskell utilizzavano funzioni come `map toLower` su stringhe, ma queste non gestivano bene la localizzazione e i caratteri speciali.

Utilizzare `Data.Text` implica anche migliori performance per le stringhe grandi, in quanto è ottimizzato per gestire grandi blocchi di testo rispetto alle stringhe semplici che sono liste di caratteri. Tuttavia, `unpack` e `pack` sono necessari per convertire tra `String` (una lista di caratteri) e `Text`.

Un'alternativa più diretta, benché meno efficiente ed elegante, impiega la funzione `toLower` del modulo `Data.Char`:

```haskell
import Data.Char (toLower)

toLowerCaseSimple :: String -> String
toLowerCaseSimple = map toLower

main :: IO ()
main = putStrLn $ toLowerCaseSimple "Ciao Mondo!"
```

Le operazioni di conversione di case sono complesse in Haskell, come in altri linguaggi, a causa della vasta gamma di regole di capitalizzazione di lettere in diverse lingue e set di caratteri (Unicode). `Data.Text.toLower` gestisce molte di queste complicazioni in modo trasparente.

## See Also
- Documentazione di `Data.Text`: https://hackage.haskell.org/package/text-1.2.4.1/docs/Data-Text.html
- Documentazione di `Data.Char`: https://hackage.haskell.org/package/base-4.16.1.0/docs/Data-Char.html
- Una guida alle stringhe in Haskell: http://haskell.org/haskellwiki/String_cheat_sheet
