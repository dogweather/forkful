---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:07:26.455493-07:00
description: "\xC5 sjekke om en mappe eksisterer er en grunnleggende operasjon i mange\
  \ programmeringsoppgaver, som tillater betingede handlinger basert p\xE5 tilstedev\xE6\
  relsen\u2026"
lastmod: '2024-03-13T22:44:40.857246-06:00'
model: gpt-4-0125-preview
summary: "\xC5 sjekke om en mappe eksisterer er en grunnleggende operasjon i mange\
  \ programmeringsoppgaver, som tillater betingede handlinger basert p\xE5 tilstedev\xE6\
  relsen eller frav\xE6ret av katalogstrukturer."
title: Sjekker om en mappe eksisterer
weight: 20
---

## Hvordan:
Haskell, gjennom sitt basebibliotek, tilbyr enkle måter å sjekke for eksistensen av mapper, hovedsakelig ved å bruke `System.Directory`-modulen. La oss se på et grunnleggende eksempel:

```haskell
import System.Directory (doesDirectoryExist)

main :: IO ()
main = do
  let dirPath = "/sti/til/din/mappe"
  exists <- doesDirectoryExist dirPath
  putStrLn $ "Eksisterer mappen? " ++ show exists
```

Eksempel på utskrift, avhengig av om mappen eksisterer:

```
Eksisterer mappen? True
```
Eller:
```
Eksisterer mappen? False
```

For mer komplekse scenarier eller ytterligere funksjonalitet, kan du vurdere et populært tredjepartsbibliotek som `filepath` for håndtering og manipulering av filstier på en mer abstrakt måte. Men, til formålet med å simpelthen sjekke om en mappe eksisterer, er basebibliotekets `System.Directory` tilstrekkelig og effektiv.

Husk, arbeid med filsystemer kan variere på tvers av plattformer, og Haskell sin tilnærming sikter mot å abstrahere bort noen av disse forskjellene. Test alltid filoperasjonene dine på målsystemet for å sikre forventet oppførsel.
