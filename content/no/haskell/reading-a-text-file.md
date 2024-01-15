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

## Hvorfor

Hvis du er interessert i funksjonell programmering eller ønsker å lære et nytt programmeringsspråk, kan det å lese en tekstfil være en nyttig øvelse. Denne artikkelen vil vise deg hvordan du kan lese en tekstfil ved hjelp av Haskell, et kraftig og elegant språk som er kjent for sin funksjonelle tilnærming til problemløsning.

## Slik gjør du det

For å lese en tekstfil i Haskell, trenger du først å åpne den ved hjelp av Haskell I/O biblioteket. La oss si at vi har en tekstfil kalt "demo.txt" som inneholder følgende linjer:

```
Dette er en testfil.
Den inneholder noen linjer med tekst.
Vi ønsker å lese denne filen ved hjelp av Haskell.
```

For å lese denne filen, må vi først importere I/O biblioteket ved å legge til følgende linje øverst i filen vår:

```
import System.IO
```

Neste trinn er å åpne filen ved hjelp av `openFile` funksjonen og angi at vi ønsker å lese den ved hjelp av `ReadMode` parameteren. Dette gjøres ved å bruke det `do` notatet, som lar oss kombinere flere I/O handlinger. Her er et eksempel på hvordan det kan se ut:

```
do
handle <- openFile "demo.txt" ReadMode
```

Kjører denne koden vil gi oss en håndterer (en verdi som representerer filen vi åpnet), som vi kan bruke til å utføre flere handlinger. For å lese innholdet i filen, kan vi bruke `hGetContents` funksjonen og lagre resultatet i en variabel. Så, for eksempel:

```
do
handle <- openFile "demo.txt" ReadMode
contents <- hGetContents handle
```

Vi kan nå bruke `contents` variabelen for å få tilgang til filens innhold. Vi kan for eksempel skrive det ut i konsollen ved å bruke `putStrLn` funksjonen:

```
do
handle <- openFile "demo.txt" ReadMode
contents <- hGetContents handle
putStrLn contents
```

Output vil da være:

```
Dette er en testfil.
Den inneholder noen linjer med tekst.
Vi ønsker å lese denne filen ved hjelp av Haskell.
```

## Dykk dypere

Nå som vi vet hvordan vi kan lese en fil i Haskell, la oss se litt nærmere på hvordan koden faktisk fungerer. Ved å bruke `openFile` funksjonen, åpner vi filen og får en handle tilbake. Dette er en måte å representere filen vår på i koden vår. Deretter bruker vi `hGetContents` funksjonen for å få til innholdet i filen, som er en streng. Vi kan deretter bruke denne strengen som vi vil, for eksempel å skrive den ut, behandle den eller lagre den i en annen fil.

Det er også verdt å merke seg at når vi er ferdige med å jobbe med filen, må vi lukke den ved hjelp av `hClose` funksjonen. Dette er viktig for å unngå eventuelle problemer med filhåndtering. Så, på slutten av koden vår, bør vi legge til:

```
do
handle <- openFile "demo.txt" ReadMode
contents <- hGetContents handle
putStrLn contents
hClose handle
```

## Se også

- [Haskell I/O Dokumentasjon](https://hackage.haskell.org/package/base/docs/System-IO.html)
- [Haskell Tutorial](https://wiki.haskell.org/Tutorials)
- [Haskell Programming Language](https://www.haskell.org/)