---
title:                "Skriving av en tekstfil"
date:                  2024-01-19
html_title:           "Arduino: Skriving av en tekstfil"
simple_title:         "Skriving av en tekstfil"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Skrive en tekstfil med Haskell handler om å lagre tekstdata til en fil. Dette gjør programmerere for å lagre innstillinger, resultatdata eller dele informasjon.

## Hvordan:

```Haskell
import System.IO

main :: IO ()
main = do
  let dataToWrite = "Hei, Norge!"
  writeFile "hei.txt" dataToWrite
  putStrLn "Fil skrevet!"

{- Kjører du programmet, skriver det "Hei, Norge!" til hei.txt og printer "Fil skrevet!" til terminalen. -}
```

## Dypdykk:

Å skrive til filer i Haskell har vært en del av standardbiblioteket lenge. Alternativer inkluderer lavnivå I/O-operasjoner med `System.IO`-biblioteket, som `openFile`, `hPutStr`, `hClose`, for mer kontroll. `writeFile` håndterer åpning, skriving, og lukking for deg, hvilket gjør det enklere for enkel bruk.

## Se Også:

- [Haskell Dokumentasjon for `writeFile`](https://hackage.haskell.org/package/base-4.16.0.0/docs/Prelude.html#v:writeFile)
- [Haskell IO Tutorial](http://learnyouahaskell.com/input-and-output)
- ["Real World Haskell" Bok](http://book.realworldhaskell.org/)
