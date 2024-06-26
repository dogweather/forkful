---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:33:15.981345-07:00
description: "Hvordan: I Haskell er det enkelt \xE5 skrive til stderr med basebibliotekets\
  \ `System.IO`-modul. Nedenfor er et grunnleggende eksempel for \xE5 demonstrere."
lastmod: '2024-03-13T22:44:40.859624-06:00'
model: gpt-4-0125-preview
summary: "I Haskell er det enkelt \xE5 skrive til stderr med basebibliotekets `System.IO`-modul."
title: Skriving til standardfeil
weight: 25
---

## Hvordan:
I Haskell er det enkelt å skrive til stderr med basebibliotekets `System.IO`-modul. Nedenfor er et grunnleggende eksempel for å demonstrere:

```haskell
import System.IO

main :: IO ()
main = do
  hPutStrLn stderr "Dette er en feilmelding."
```

Utdataene fra dette programmet til stderr ville være:

```
Dette er en feilmelding.
```

Hvis du jobber i en mer kompleks applikasjon, eller hvis du trenger bedre kontroll over logging (inkludert feil), kan du velge et tredjepartsbibliotek. Et populært valg er `monad-logger` som integrerer med `mtl`-stilen til Haskell-programmering. Her er et lite utdrag som bruker `monad-logger`:

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Control.Monad.Logger

main :: IO ()
main = runStderrLoggingT $ do
  logErrorN "Dette er en feilmelding som bruker monad-logger."
```

Når den kjøres, gir `monad-logger`-versjonen på samme måte ut en feilmelding, men den er utstyrt med mer kontekst, som tidsstempler eller loggnivåer, avhengig av konfigurasjonen:

```
[Error] Dette er en feilmelding som bruker monad-logger.
```

Begge metodene tjener formålet med å skrive til stderr, med valget som i stor grad avhenger av kompleksiteten og behovene til applikasjonen din.
