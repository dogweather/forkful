---
title:                "Att använda reguljära uttryck"
aliases:
- /sv/haskell/using-regular-expressions/
date:                  2024-02-03T19:16:57.965311-07:00
model:                 gpt-4-0125-preview
simple_title:         "Att använda reguljära uttryck"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/using-regular-expressions.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Vad & Varför?
Reguljära uttryck i programmering är sekvenser av tecken som definierar ett sökmönster, typiskt använda för att söka och manipulera strängar. Haskell-programmerare använder reguljära uttryck för uppgifter som sträcker sig från enkel strängmatchning till komplex textbearbetning, och utnyttjar deras effektivitet och mångsidighet när det gäller hantering av textdata.

## Hur:
I Haskell är regex-funktionalitet inte en del av standardbiblioteket, vilket kräver användning av tredjeparts paket såsom `regex-base` tillsammans med ett kompatibelt backend som `regex-posix` (för POSIX regex-stöd), `regex-pcre` (för Perl-kompatibelt regex), osv. Så här kan du använda dessa paket för att arbeta med reguljära uttryck.

Först, se till att du har paketen installerade genom att lägga till `regex-posix` eller `regex-pcre` i ditt projekts `.cabal`-fil eller installera via cabal direkt:

```bash
cabal install regex-posix
```
eller
```bash
cabal install regex-pcre
```

### Använda `regex-posix`:

```haskell
import Text.Regex.Posix ((=~))

-- Kontrollera om en sträng matchar ett mönster
isMatch :: String -> String -> Bool
isMatch text mönster = text =~ mönster :: Bool

-- Hitta första matchen
findFirst :: String -> String -> String
findFirst text mönster = text =~ mönster :: String

main :: IO ()
main = do
    print $ isMatch "hello world" "wo"
    -- Utdata: True
    print $ findFirst "good morning, good night" "good"
    -- Utdata: "good"
```

### Använda `regex-pcre`:

```haskell
import Text.Regex.PCRE ((=~))

-- Hitta alla matchningar
findAll :: String -> String -> [String]
findAll text mönster = text =~ mönster :: [String]

main :: IO ()
main = do
    print $ findAll "test1 test2 test3" "\\btest[0-9]\\b"
    -- Utdata: ["test1","test2","test3"]
```

Varje bibliotek har sina särdrag, men den allmänna metoden att använda `=~` för att applicera regex förblir konsekvent, oavsett om det handlar om att kontrollera för en matchning eller extrahera delsträngar. Valet mellan `regex-posix` och `regex-pcre` beror i stor utsträckning på ditt projekts behov och de specifika regex-funktioner som krävs.
