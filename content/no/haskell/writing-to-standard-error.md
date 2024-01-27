---
title:                "Skrive til standardfeil"
date:                  2024-01-19
html_title:           "Arduino: Skrive til standardfeil"
simple_title:         "Skrive til standardfeil"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why?
Standard error er en separat utdatastrøm for å melde feil og logge. Programmerere bruker den slik at feil og standard utdata kan håndteres separat.

## How to:
```Haskell
import System.IO

main :: IO ()
main = do
  hPutStrLn stderr "Dette vises i standard error."
  putStrLn "Dette vises i standard output."
```
Forventet utdata på terminalen hvis du omadresserer standard output til en fil:
```
$ runhaskell example.hs > output.txt
Dette vises i standard error.
```
Innholdet av `output.txt` vil være:
```
Dette vises i standard output.
```

## Deep Dive
Standard error, ofte referert til som `stderr`, er en av de tre hoveddatastrømmene brukt siden Unix-tiden. De andre er standard input (`stdin`) og standard output (`stdout`). I Haskell håndterer vi `stderr` med `System.IO` biblioteket. 

En alternativ måte å håndtere feil på er å bruke unntak, men ved å skrive direkte til `stderr` kan man lettere sende feilsøkingsmeldinger til konsollen mens vanlig output går til filer eller andre strømmer.

`hPutStrLn` funksjonen skriver en streng til den angitte `Handle`, som i dette tilfellet er `stderr` istedenfor standard `stdout`.

## See Also
- Haskell `System.IO`-dokumentasjon: https://hackage.haskell.org/package/base/docs/System-IO.html 
- Haskell Wiki om IO: https://wiki.haskell.org/IO_inside
- Omstrukturering av utdatastrømmer i Unix: https://en.wikipedia.org/wiki/Standard_streams
