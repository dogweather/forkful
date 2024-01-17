---
title:                "Lesing av tekstfil"
html_title:           "Haskell: Lesing av tekstfil"
simple_title:         "Lesing av tekstfil"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Hva & hvorfor?

Lesing av en tekstfil er en vanlig oppgave for programmerere. Det handler om å lese og tolke informasjon fra en fil lagret på en datamaskin. Dette er nyttig når man ønsker å hente ut data fra en ekstern kilde, som for eksempel en database eller et API. Det kan også være nødvendig å lese en tekstfil i programmering for å utføre spesifikke oppgaver, som for eksempel å manipulere data eller å trekke ut nødvendig informasjon.

## Hvordan:

I Haskell kan vi lese en tekstfil ved å bruke funksjonen "readFile". Denne funksjonen tar inn navnet på filen som argument og returnerer innholdet av filen som en streng. La oss se et eksempel på hvordan dette kan gjøres:

```Haskell
main = do
  fileContents <- readFile "tekstfil.txt"
  putStrLn fileContents
```

Her bruker vi "putStrLn" funksjonen til å skrive ut innholdet av filen til standard output. Dette eksempelet antar at tekstfilen "tekstfil.txt" ligger i samme mappe som Haskell-filen.

Når vi kjører dette programmet, vil vi få utskriften av filens innhold på skjermen.

## Dypdykk:

Historisk sett var lesing av tekstfiler en viktig oppgave i programmering, spesielt når det gjaldt å lese og tolke data fra eksterne kilder. I dag er det imidlertid flere alternativer for å lese og håndtere data i programmering, som for eksempel bruk av databaser eller nettverkskommunikasjon gjennom API-er.

Når det gjelder utførelse og implementasjon av lesing av tekstfiler, avhenger det av språk og biblioteker som brukes. I Haskell brukes "readFile" funksjonen fra standardbiblioteket "System.IO" til å lese filer. Det finnes også andre funksjoner og biblioteker tilgjengelig for lesing og håndtering av tekstfiler i Haskell.

## Se også:

- [Haskell Dokumentasjon](https://www.haskell.org/documentation/)
- [Haskell Biblioteker](https://hackage.haskell.org/)
- [Haskell Programmeringsspråk](https://www.haskell.org/)