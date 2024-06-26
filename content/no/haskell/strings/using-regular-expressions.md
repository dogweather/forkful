---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:16:52.920660-07:00
description: "Hvordan: I Haskell er ikke regex-funksjonaliteter en del av det standard\
  \ biblioteket, noe som n\xF8dvendiggj\xF8r bruk av tredjepartspakker som `regex-base`\u2026"
lastmod: '2024-03-13T22:44:40.831004-06:00'
model: gpt-4-0125-preview
summary: "I Haskell er ikke regex-funksjonaliteter en del av det standard biblioteket,\
  \ noe som n\xF8dvendiggj\xF8r bruk av tredjepartspakker som `regex-base` sammen\
  \ med en kompatibel bakpart som `regex-posix` (for POSIX regex-st\xF8tte), `regex-pcre`\
  \ (for Perl-kompatible regex), osv."
title: "Bruke regul\xE6re uttrykk"
weight: 11
---

## Hvordan:
I Haskell er ikke regex-funksjonaliteter en del av det standard biblioteket, noe som nødvendiggjør bruk av tredjepartspakker som `regex-base` sammen med en kompatibel bakpart som `regex-posix` (for POSIX regex-støtte), `regex-pcre` (for Perl-kompatible regex), osv. Slik kan du bruke disse pakkene for å arbeide med regulære uttrykk.

Først, sørg for at du har installert pakkene ved å legge til `regex-posix` eller `regex-pcre` i prosjektets `.cabal`-fil eller installere via cabal direkte:

```bash
cabal install regex-posix
```
eller
```bash
cabal install regex-pcre
```

### Bruke `regex-posix`:
```haskell
import Text.Regex.Posix ((=~))

-- Sjekk om en streng matcher et mønster
isMatch :: String -> String -> Bool
isMatch tekst mønster = tekst =~ mønster :: Bool

-- Finn det første treffet
findFirst :: String -> String -> String
findFirst tekst mønster = tekst =~ mønster :: String

main :: IO ()
main = do
    print $ isMatch "hello world" "wo"
    -- Utdata: True
    print $ findFirst "god morgen, god natt" "god"
    -- Utdata: "god"
```

### Bruke `regex-pcre`:
```haskell
import Text.Regex.PCRE ((=~))

-- Finn alle treff
findAll :: String -> String -> [String]
findAll tekst mønster = tekst =~ mønster :: [String]

main :: IO ()
main = do
    print $ findAll "test1 test2 test3" "\\btest[0-9]\\b"
    -- Utdata: ["test1","test2","test3"]
```

Hver bibliotek har sine spesifikke egenskaper, men den generelle metoden å bruke `=~` for å anvende regex forblir konsistent, enten det er for å sjekke for et treff eller ekstrahere delstrenger. Valget mellom `regex-posix` eller `regex-pcre` avhenger i stor grad av ditt prosjekts behov og de spesifikke regex-egenskapene som kreves.
