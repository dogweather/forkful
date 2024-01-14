---
title:    "Haskell: Kontrollerer om en mappe eksisterer"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/haskell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Hvorfor

Når du jobber med å utvikle programmer, kan det være nyttig å sjekke om en mappe eksisterer før du prøver å utføre en operasjon på den. Dette hjelper deg å unngå feil og sikrer at programmet ditt kjører jevnt.

## Hvordan

Å sjekke om en mappe eksisterer, kan enkelt gjøres ved å bruke funksjonen `doesDirectoryExist` fra Haskell-modulen `System.Directory`. Denne funksjonen tar inn en `FilePath` som argument, som er banen til mappen du vil sjekke. Her er et eksempel på hvordan du kan bruke denne funksjonen:

```Haskell
import System.Directory

-- Sjekker om mappen "docs" eksisterer
doesDirectoryExist "docs"
```

Hvis mappen eksisterer, vil funksjonen returnere `True`, ellers vil den returnere `False`. Du kan også bruke en ternary operator for å få en enklere utskrift:

```Haskell
import System.Directory

-- Sjekker om mappen "docs" eksisterer og printer ut en melding
let dir = if doesDirectoryExist "docs" then "Mappen finnes!" else "Mappen finnes ikke!"
print dir
```

Output vil være enten "Mappen finnes!" eller "Mappen finnes ikke!".

## Dypdykk

Hvis du ønsker å sjekke om en mappe eksisterer på en spesifikk bane, kan du bruke funksjonen `doesPathExist` i stedet. Denne funksjonen kan ta inn hvilken som helst `FilePath`, selv om den ikke er en mappe. Den sjekker om filen eller mappen eksisterer på den angitte banen.

```Haskell
import System.Directory

-- Sjekker om banen "/Brukere/navn/dokumenter" eksisterer
doesPathExist "/Users/navn/Documents"
```

Output vil være enten `True` eller `False`.

## Se også

- [System.Directory dokumentasjon](https://hackage.haskell.org/package/directory/docs/System-Directory.html)
- [Dokumentasjon for `doesDirectoryExist` og `doesPathExist` funksjonene](https://hackage.haskell.org/package/directory/docs/System-Directory.html#v:doesDirectoryExist)