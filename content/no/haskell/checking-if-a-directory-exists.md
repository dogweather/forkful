---
title:                "Haskell: Å sjekke om en mappe eksisterer"
simple_title:         "Å sjekke om en mappe eksisterer"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

"## Hvorfor"

Å sjekke om en mappe eksisterer kan være en viktig del av å lage pålitelige og funksjonelle programmer i Haskell. Det kan også hjelpe til med å organisere og håndtere datafiler på en effektiv måte.

"## Hvordan"

For å sjekke om en mappe eksisterer, kan vi bruke funksjonen `doesDirectoryExist` fra `System.Directory`-modulen. Denne funksjonen tar inn en streng som representerer stien til mappen, og returnerer en `Bool`-verdi som indikerer om mappen eksisterer eller ikke.

```Haskell
import System.Directory  -- importerer modulen

hoved :: IO ()
hoved = do
  eksisterer <- doesDirectoryExist "sti/til/mappe" -- sjekker om mappen eksisterer
  if eksisterer
    then putStrLn "Mappen eksisterer!"  -- hvis mappen eksisterer, skriv ut melding
    else putStrLn "Mappen eksisterer ikke." -- hvis mappen ikke eksisterer, skriv ut melding
```
For å få en bedre forståelse av hvordan funksjonen `doesDirectoryExist` fungerer, kan vi også se på et eksempel på hvordan den håndterer feil. Hvis vi prøver å sjekke om en mappe som ikke eksisterer, vil funksjonen gi oss en `IOException`-feil. Vi kan håndtere denne feilen ved å bruke funksjonen `catch` fra `Control.Exception`-modulen.

```Haskell
import System.Directory
import Control.Exception

hoved :: IO ()
hoved = do
  result <- try $ doesDirectoryExist "sti/til/eksisterende/mappe" :: IO (Either IOException Bool)
  case result of
    Left _ -> putStrLn "Kunne ikke sjekke mappen."  -- håndterer IOException-feilen
    Right eksisterer -> if eksisterer
      then putStrLn "Mappen eksisterer!"
      else putStrLn "Mappen eksisterer ikke."
```

"## Dykk dypere"

For å kunne sjekke om en mappe eksisterer, bruker funksjonen `doesDirectoryExist` systemkallene `access` og `lstat` fra C. Dette betyr at funksjonen ikke bare sjekker om filen eksisterer, men også om brukeren har tilgang til mappen. Dette kan være nyttig for å sikre at programmet bare jobber med filer og mapper som tilhører brukeren, eller for å unngå uønskede operasjoner.

"## Se også"

- [System.Directory-dokumentasjon](https://hackage.haskell.org/package/directory/docs/System-Directory.html)
- [Control.Exception-dokumentasjon](https://hackage.haskell.org/package/base/docs/Control-Exception.html)
- [Learn You a Haskell-kapittel om håndtering av feil](https://learnyouahaskell.com/for-a-few-monads-more#exceptions)