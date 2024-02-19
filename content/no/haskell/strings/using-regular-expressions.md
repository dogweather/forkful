---
aliases:
- /no/haskell/using-regular-expressions/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:16:52.920660-07:00
description: "Regul\xE6re uttrykk i programmering er sekvenser av tegn som definerer\
  \ et s\xF8kem\xF8nster, typisk brukt for s\xF8k i strenger og manipulering av disse.\
  \ Haskell-\u2026"
lastmod: 2024-02-18 23:08:53.925259
model: gpt-4-0125-preview
summary: "Regul\xE6re uttrykk i programmering er sekvenser av tegn som definerer et\
  \ s\xF8kem\xF8nster, typisk brukt for s\xF8k i strenger og manipulering av disse.\
  \ Haskell-\u2026"
title: "Bruke regul\xE6re uttrykk"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Regulære uttrykk i programmering er sekvenser av tegn som definerer et søkemønster, typisk brukt for søk i strenger og manipulering av disse. Haskell-programmerere bruker regulære uttrykk til oppgaver som spenner fra enkel strengmatching til kompleks tekstbehandling, ved å utnytte deres effektivitet og allsidighet når det gjelder behandling av tekstdata.

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
