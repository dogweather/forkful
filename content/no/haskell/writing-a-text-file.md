---
title:    "Haskell: Å skrive en tekstfil"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/haskell/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Hvorfor
Hvis du er en programmerer eller en nybegynner som ønsker å lære et funksjonelt programmeringsspråk, kan Haskell være et bra alternativ. En av de mange tingene du kan gjøre i Haskell er å skrive tekstfiler. I denne artikkelen vil vi utforske hvorfor og hvordan du kan gjøre dette.

## Hvordan
La oss starte med et enkelt eksempel på hvordan du kan skrive en tekstfil i Haskell. Vi vil bruke hovedfunksjonen "main" som starter programmet vårt og "writeFile" som lar oss skrive en ny fil.

```Haskell
main = do
  writeFile "hello.txt" "Hei, verden!"
```

Når du kjører dette programmet, vil du få en ny fil som heter "hello.txt" med innholdet "Hei, verden!". Dette er et enkelt eksempel, men du kan gjøre mer avansert tekstbehandling ved å bruke innebygde funksjoner og biblioteker.

## Dypdykk
Nå som vi har sett et enkelt eksempel på hvordan skrive en tekstfil, la oss ta en dypere titt på hvordan dette fungerer. Først og fremst må du importere "System.IO" biblioteket for å bruke funksjonen "writeFile". Deretter kan du bruke "do" notasjon for å kjøre flere IO-operasjoner i en sekvens.

```Haskell
import System.IO

main = do
  handle <- openFile "hello.txt" WriteMode
  hPutStrLn handle "Hei, verden!"
  hClose handle
```

I dette eksempelet åpner vi en fil i "WriteMode" og tildeler det til en "handle" variabel. Deretter bruker vi "hPutStrLn" for å skrive teksten til filen og til slutt lukker vi filen med "hClose".

## Se også
- [Dokumentasjon for Haskell I/O](https://hackage.haskell.org/package/base-4.15.0.0/docs/System-IO.html)
- [LæreHaskell - En ressurs for å lære Haskell på norsk](https://www.larehaskell.nu/)
- [Offisiell Haskell-nettside](https://www.haskell.org/)