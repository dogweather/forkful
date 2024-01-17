---
title:                "Kontrollere om en mappe eksisterer"
html_title:           "Haskell: Kontrollere om en mappe eksisterer"
simple_title:         "Kontrollere om en mappe eksisterer"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å sjekke om en mappe eksisterer er en vanlig praksis blant programmere. Dette refererer til å sjekke om en gitt mappe eller et gitt sti eksisterer i filsystemet på datamaskinen. Dette er nyttig for å sikre at programmet vårt kan håndtere tilfeller der en mappe mangler eller ikke er tilgjengelig.

## Slik gjør du det:

For å sjekke om en mappe eksisterer i Haskell, kan vi bruke funksjonen `doesDirectoryExist` fra `System.Directory` biblioteket. Denne funksjonen tar inn en sti som en streng og returnerer en boolsk verdi som indikerer om mappen eksisterer eller ikke.

```Haskell
import System.Directory

main = do
  isExist <- doesDirectoryExist "test"
  if isExist
    then putStrLn "Mappen finnes!"
    else putStrLn "Mappen finnes ikke!"
```

Output:

```
Mappen finnes ikke!
```

## Dypdykk:

Å sjekke om en mappe eksisterer kan være nyttig når vi ønsker å utføre spesifikke handlinger bare hvis en mappe finnes. Det kan også være nyttig å validere brukerinput for å sikre at en mappe faktisk finnes før vi forsøker å arbeide med den.

En alternativ måte å sjekke om en mappe eksisterer på er gjennom å bruke funksjonen `doesDirectoryExist` fra `System.Posix.Directory` biblioteket. Denne funksjonen kjører på POSIX-systemer og returnerer også en boolsk verdi.

Når det gjelder implementasjon, bruker funksjonen `doesDirectoryExist` en kombinasjon av `stat` og `isDirectory` funksjonene fra filbehandlingssystemet for å sjekke om stien peker på en mappe eller ikke. Dette gjør den mer pålitelig enn bare å sjekke om stien er en gyldig mappe, da det kan være tilfeller der en gyldig mappe inneholder filer.

## Se også:

- [Dokumentasjon for `System.Directory` biblioteket](https://hackage.haskell.org/package/directory/docs/System-Directory.html)
- [Dokumentasjon for `System.Posix.Directory` biblioteket](https://hackage.haskell.org/package/directory/docs/System-Posix-Directory.html)