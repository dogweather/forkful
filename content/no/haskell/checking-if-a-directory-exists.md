---
title:    "Haskell: Sjekke om en mappe eksisterer"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Hvorfor

Å sjekke om en mappe eksisterer kan være nyttig når du jobber med filbehandling i Haskell. Dette kan hjelpe deg med å unngå feil og håndtere ulike grensetilfeller når du skal åpne eller lagre filer.

## Hvordan

For å sjekke om en mappe eksisterer i Haskell, kan du bruke funksjonen `doesDirectoryExist` fra `System.Directory` modulen. Denne funksjonen tar inn en `FilePath` - som er en streng som representerer plasseringen til mappen - og returnerer en `Bool` som indikerer om mappen eksisterer eller ikke.

```Haskell
import System.Directory (doesDirectoryExist)

-- Sjekker om mappen "Dokumenter" eksisterer
doesDirectoryExist "Dokumenter"
-- Output: True

-- Sjekker om mappen "Bilder" eksisterer
doesDirectoryExist "Bilder"
-- Output: False
```

## Dypdykk

Hvis du vil lære mer om hvordan funksjonen `doesDirectoryExist` faktisk fungerer bak kulissene, kan du ta en titt på dokumentasjonen eller kildekoden til `System.Directory` modulen. Der vil du kunne se at denne funksjonen bruker systemkallet `stat` for å få informasjon om filen eller mappen som gitt `FilePath` peker på.

Dette systemkallet er en del av POSIX standarden, som er en samling av standarder for Unix-baserte operativsystemer som inkluderer Linux og macOS.

## Se også

* [System.Directory dokumentasjon](https://hackage.haskell.org/package/directory/docs/System-Directory.html)
* [System.Directory kildekode ](https://hackage.haskell.org/package/directory/docs/src/System-Directory.html#stat)