---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:33:15.981345-07:00
description: "\xC5 skrive til standard error (stderr) i Haskell lar programmer skille\
  \ utdataene sine mellom vanlige resultater og feilmeldinger. Dette er avgj\xF8rende\
  \ for \xE5\u2026"
lastmod: 2024-02-19 22:05:00.122952
model: gpt-4-0125-preview
summary: "\xC5 skrive til standard error (stderr) i Haskell lar programmer skille\
  \ utdataene sine mellom vanlige resultater og feilmeldinger. Dette er avgj\xF8rende\
  \ for \xE5\u2026"
title: Skriving til standardfeil
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å skrive til standard error (stderr) i Haskell lar programmer skille utdataene sine mellom vanlige resultater og feilmeldinger. Dette er avgjørende for å signalisere problemer og for feilsøking, uten å rotetil det standard utdata (stdout) som ofte bærer programmets primære data eller resultat.

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
