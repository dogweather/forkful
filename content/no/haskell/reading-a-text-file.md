---
title:                "Haskell: Å lese en tekstfil"
simple_title:         "Å lese en tekstfil"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Hvorfor
Velkommen til min blogg om Haskell-programmering! I dag skal vi snakke om lesing av tekstfiler i Haskell. Så hvorfor skal du bry deg om dette? Vel, som en programmerer, vil du ofte ha behov for å lese data fra en tekstfil for å behandle det videre i programmet ditt. Derfor er det viktig å forstå hvordan man leser tekstfiler i Haskell.

## Hvordan
For å lese en tekstfil i Haskell, må vi først åpne filen og deretter lese innholdet. Vi kan gjøre dette ved hjelp av "openFile" funksjonen. La oss se på et eksempel:

```
main = do
    handle <- openFile "tekstfil.txt" ReadMode
    contents <- hGetContents handle
    putStrLn contents
    hClose handle
```

Vi åpner her filen "tekstfil.txt" og bruker "hGetContents" funksjonen for å lese innholdet og lagre det i variabelen "contents". Deretter skriver vi ut innholdet ved hjelp av "putStrLn" funksjonen. Til slutt lukker vi filen ved å bruke "hClose" funksjonen. Ved å kjøre denne koden, vil vi få utskrevet innholdet i tekstfilen på skjermen.

## Dypdykk
La oss nå se nærmere på hvordan "hGetContents" funksjonen fungerer. Denne funksjonen returnerer en streng med hele innholdet i filen, men den leser ikke alt på en gang. I stedet leser den innholdet i filen etter behov, dvs når vi kaller på "putStrLn" funksjonen. Dette gjør at vi kan lese store filer uten å belaste minnet unødvendig.

En annen tilnærming for å lese tekstfiler i Haskell er å bruke "readFile" funksjonen. Denne funksjonen tar inn filnavnet som parameter og returnerer også en streng med innholdet i filen. Forskjellen er at "readFile" funksjonen leser hele filen på en gang og lagrer det i minnet. Derfor kan dette føre til problemer hvis filen er veldig stor.

## Se også
- [Haskell dokumentasjon om lesing av tekstfiler](https://hackage.haskell.org/package/base-4.14.1.0/docs/System-IO.html#v:hGetContents)
- [En tutorial om lesing av tekstfiler i Haskell](https://www.tutorialspoint.com/haskell/haskell_reading_files.htm)
- [En utfyllende forklaring om hvordan "hGetContents" funksjonen fungerer](https://wiki.haskell.org/I/O_utilities#Function_hGetContents)