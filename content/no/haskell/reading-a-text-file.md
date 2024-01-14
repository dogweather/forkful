---
title:    "Haskell: Lese en tekstfil"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Hvorfor

Å lese og behandle tekstfiler er en vanlig oppgave for mange programmører, uavhengig av hvilket språk de jobber med. I Haskell er det flere måter å lese en tekstfil på, og i denne artikkelen vil vi se nærmere på hvordan dette kan gjøres.

## Hvordan

For å lese en tekstfil i Haskell, må vi først importere modulen `System.IO`. Dette gjøres med følgende linje:

```Haskell
import System.IO
```

Deretter kan vi bruke funksjonen `readFile` for å lese innholdet i en tekstfil. Denne funksjonen tar inn en streng som representerer filnavnet, og returnerer innholdet i filen som en annen streng.

```Haskell
fileContent <- readFile "tekstfil.txt"
```

Vi kan også bruke funksjonen `hGetContents` for å lese innholdet i en fil basert på en håndteringsstrøm (handle). Dette kan være nyttig hvis vi har behov for å lese fra en bestemt del av filen eller fra flere filer samtidig.

```Haskell
handle <- openFile "tekstfil.txt" ReadMode
fileContent <- hGetContents handle
```

Etter at vi har lest innholdet fra filen, kan vi behandle det som en vanlig streng i Haskell, ved for eksempel å splitte eller manipulere den på ulike måter. Vi kan også bruke funksjoner fra modulen `Data.Text` for å arbeide med tekstfiler på en mer effektiv måte.

Etter at vi er ferdig med å lese filen, må vi huske å lukke håndteringsstrømmen ved å bruke funksjonen `hClose`.

```Haskell
hClose handle
```

## Dypdykk

I Haskell er det også mulig å lese en tekstfil linje for linje ved hjelp av funksjonene `hGetLine` og `hIsEOF`. Funksjonen `hGetLine` tar inn en håndteringsstrøm og returnerer neste linje i filen som en streng. Funksjonen `hIsEOF` sjekker om håndteringsstrømmen har nådd slutten av filen.

```Haskell
whileM_ (not <$> hIsEOF handle) $ do
    line <- hGetLine handle
    putStrLn line
```

I dette eksempelet bruker vi funksjonen `whileM_` fra modulen `Control.Monad.Loops` for å gjenta lesingen til håndteringsstrømmen når slutten av filen. Deretter bruker vi funksjonen `putStrLn` for å skrive ut hver linje som vi leser.

## Se også

- [Haskell.org - Reading and writing files](https://www.haskell.org/tutorial/iomonad.html#reading-and-writing-files)
- [Haskell.org - Data.Text](https://hackage.haskell.org/package/text)
- [Haskell.org - System.IO](https://hackage.haskell.org/package/base-4.15.0.0/docs/System-IO.html)