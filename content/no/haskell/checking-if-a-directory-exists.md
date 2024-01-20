---
title:                "Sjekke om en mappe eksisterer"
date:                  2024-01-20T14:57:00.616639-07:00
html_title:           "Fish Shell: Sjekke om en mappe eksisterer"
simple_title:         "Sjekke om en mappe eksisterer"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Sjekke om en katalog finnes er å verifisere dens eksistens i filsystemet før vi prøver å lese eller skrive til den. Programmerere gjør dette for å unngå feil og krasj under kjøringen.

## Hvordan:
I Haskell bruker vi ofte `doesDirectoryExist` funksjonen fra `System.Directory` for å sjekke en katalogs eksistens. Her er et eksempel:

```Haskell
import System.Directory (doesDirectoryExist)

main :: IO ()
main = do
    let dir = "/path/to/directory"
    exists <- doesDirectoryExist dir
    putStrLn $ "Directory " ++ (if exists then "exists." else "does not exist.")
```

Kjører du dette kan du forvente følgende utskrift:

```
Directory exists.
```

eller

```
Directory does not exist.
```

avhengig av om katalogen eksisterer.

## Dykk Ned:
Før `doesDirectoryExist` ble tilgjengelig, måtte du kanskje bruke mer lavnivå funksjoner for å sjekke filmetainformasjon, noe som kunne bli klønete. Denne funksjonen, sammen med andre hjelpefunksjoner i `System.Directory`, tilbyr et høynivå API som er enkelt å bruke. Alternativer for å kontrollere filsystemet innebærer å bruke `System.FilePath` for å bygge sti spesifikt for operativsystemet eller å lese direkte fra filsystemet ved hjelp av rå systemkall. Implementasjonsdetaljer varierer avhengig av plattformen, men Haskell abstraherer disse forskjellene via biblioteket, slik at du kan skrive plattformuavhengig kode.

## Se Også:
- [System.Directory dokumentasjon](https://hackage.haskell.org/package/directory-1.3.6.1/docs/System-Directory.html)
- [FilePath-biblioteket dokumentasjon](https://hackage.haskell.org/package/filepath)