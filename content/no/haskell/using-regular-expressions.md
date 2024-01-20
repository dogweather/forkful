---
title:                "Bruk av regulære uttrykk"
html_title:           "Bash: Bruk av regulære uttrykk"
simple_title:         "Bruk av regulære uttrykk"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Regular expressions, eller regex, lar oss søke og manipulere tekst med et "wildcard"-språk. Programmere bruker det for å filtrere, finne, eller erstatte spesifikke mønstre i tekst.

## Hvordan:
Haskell bruker `regex`-pakker for mønstergjenkjenning. Her er en enkel brukerveiledning:

```Haskell
import Text.Regex.TDFA ((=~))

-- Finn om 'Hello, world!' inneholder ordet 'world'
main :: IO ()
main = print $ "Hello, world!" =~ "world" :: Bool

-- Resultat
True
```

Mer avanserte bruksmål:

```Haskell
import Text.Regex.TDFA

-- Finn alle ord som starter på 'h'
finnOrd :: String -> [String]
finnOrd tekst = tekst =~ "\\bh\\w*"

-- Eksempel
main :: IO ()
main = print $ finnOrd "heisann sveisann, Hva skjer?"

-- Resultat
["heisann", "Hva"]
```

## Dypdykk
Regex i Haskell er hovedsakelig håndtert av biblioteker som `regex-tdfa` og `regex-posix`. Disse bygger på eldre regex-biblioteker, men er tilpasset for Haskell og støtter avanserte regex-funksjoner som "lazy" matching og back-references.

Alternativer inkluderer å bruke innebygde strengoperasjoner eller parserbiblioteker som Parsec for mer strukturert tekstanalyse.

Implementasjonsdetaljer for regex-biblioteker i Haskell kan variere, men de fleste benytter seg av Thompson's construction algoritme for å konvertere regexer til nondeterministic finite automata (NFA), hvilket er kjent for sin effektivitet.

## Se Også
- Hackage for regex-tdfa: [https://hackage.haskell.org/package/regex-tdfa](https://hackage.haskell.org/package/regex-tdfa)
- Haskell Wiki om regex: [https://wiki.haskell.org/Regular_expressions](https://wiki.haskell.org/Regular_expressions)
- "Real World Haskell" om regex: [http://book.realworldhaskell.org/read/regular-expressions.html](http://book.realworldhaskell.org/read/regular-expressions.html)