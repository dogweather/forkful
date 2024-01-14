---
title:                "Haskell: Lesing av en tekstfil"
programming_language: "Haskell"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Hvorfor

Å lese en tekstfil er en vanlig oppgave i programmering, og kan være nyttig for å hente informasjon fra en ekstern datakilde. I Haskell, kan dette gjøres enkelt ved å bruke innebygde funksjoner og metoder.

## Slik gjør du det

For å lese en tekstfil i Haskell, må du først åpne filen ved hjelp av "openFile" funksjonen. Dette vil gi deg et "handle" objekt som representerer filen. Deretter kan du bruke funksjoner som "hGetLine" for å lese en linje av teksten om gangen, eller "hGetContents" for å lese hele teksten på én gang.

```Haskell
import System.IO

main = do
  handle <- openFile "tekstfil.txt" ReadMode
  tekst <- hGetContents handle
  putStrLn tekst

```

Koden over åpner en tekstfil ved navn "tekstfil.txt" i lesemodus, leser innholdet og skriver det ut i terminalen.

## Dykk dypere

Haskell har mange innebygde funksjoner som gjør det enkelt å jobbe med tekstfiler. For eksempel kan du bruke "withFile" funksjonen for å sikre at filen lukkes etter at du er ferdig med å lese den. Du kan også bruke funksjoner som "hGetChar" for å lese et tegn om gangen, eller "hIsEOF" for å sjekke om du har nådd slutten av filen.

Men det er også mulig å lese tekstfiler på en mer avansert måte ved å bruke funksjoner som "lines" for å lese hver linje inn i en liste, eller "words" for å lese hvert ord inn i en annen liste. Dette kan være nyttig hvis du trenger å manipulere dataene før du skriver dem ut.

## Se også

- [Offisiell Haskell dokumentasjon](https://www.haskell.org/documentation/)
- [Haskell Wikibook](https://en.wikibooks.org/wiki/Haskell)
- [Haskell Programmeringsspråk](https://www.haskell.org/)