---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:28:06.137984-07:00
description: "Hvordan: Haskells standard Prelude tilbyr grunnleggende st\xF8tte for\
  \ skriving til filer ved bruk av `writeFile` og `appendFile` funksjonene fra `System.IO`\u2026"
lastmod: '2024-03-13T22:44:40.861743-06:00'
model: gpt-4-0125-preview
summary: "Haskells standard Prelude tilbyr grunnleggende st\xF8tte for skriving til\
  \ filer ved bruk av `writeFile` og `appendFile` funksjonene fra `System.IO` modulen."
title: Skrive en tekstfil
weight: 24
---

## Hvordan:
Haskells standard Prelude tilbyr grunnleggende støtte for skriving til filer ved bruk av `writeFile` og `appendFile` funksjonene fra `System.IO` modulen. Her er et grunnleggende eksempel på å opprette en ny fil (eller overskrive en eksisterende) og deretter legge til tekst i en fil.

```haskell
import System.IO

-- Skrive til en fil, overskriver hvis den eksisterer
main :: IO ()
main = do
  writeFile "example.txt" "Dette er linje en.\n"
  appendFile "example.txt" "Dette er linje to.\n"
```

Når du kjører dette programmet, opprettes (eller tømmes) `example.txt` og skriver "Dette er linje en." etterfulgt av "Dette er linje to." på neste linje.

For mer avansert filhåndtering, vender Haskell-programmerere ofte til `text` pakken for effektiv strengbehandling og `bytestring` pakken for håndtering av binærdata. Slik bruker du `text` pakken for fil-IO:

Først må du legge til `text` i prosjektets avhengigheter. Deretter kan du bruke den som følger:

```haskell
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

-- Skrive til en fil ved bruk av text-pakken
main :: IO ()
main = do
  let innhold = T.pack "Bruker text-pakken for bedre ytelse.\n"
  TIO.writeFile "textExample.txt" innhold
  TIO.appendFile "textExample.txt" $ T.pack "Legger til linje to.\n"
```

I dette utdraget konverterer `T.pack` en vanlig `String` til `Text`-typen, som er mer effektiv. `TIO.writeFile` og `TIO.appendFile` er `text`-ekvivalentene for å skrive og legge til filer, henholdsvis.

Når du kjører denne koden, vil resultatet være en fil med navnet `textExample.txt` med to linjer med tekst, som demonstrerer både opprettelse og tilføyelse av kapasiteter ved bruk av det avanserte `text`-biblioteket for bedre ytelse og evne til å håndtere Unicode-tekst.
