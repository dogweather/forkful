---
title:                "Å bruke regulære uttrykk"
html_title:           "Haskell: Å bruke regulære uttrykk"
simple_title:         "Å bruke regulære uttrykk"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/using-regular-expressions.md"
---

{{< edit_this_page >}}

# Hva er regulære uttrykk og hvorfor bruker programmerere det?

Regulære uttrykk er et verktøy som hjelper programmere å søke og manipulere tekststrenger basert på visse mønstre. Dette gjøres ved å definere et mønster som matcher deler av tekststrengen. Programmere bruker regulære uttrykk for å effektivt gjøre operasjoner som filtrering, erstattning og ekstraksjon av informasjon fra store mengder tekst.

# Hvordan gjør du dette?

```Haskell
import Text.Regex.Posix

main :: IO ()
main = do
  let string = "Hello, world!"
  let pattern = "Hello, (.*)!" -- Dette er vårt mønster, og "world" er en "submatch"
  let result = string =~ pattern :: [[String]] -- =~ matcher mønsteret mot tekststrengen og returnerer en liste med treff
  print result -- [["Hello, world!","world"]]
```

# Dykk ned i detaljene

(1) Regulære uttrykk har vært en del av programmering siden 1950-tallet, da det ble utviklet som en del av matematisk teori. (2) Alternativer som ligner på regulære uttrykk inkluderer "glob patterns" og "wildcards". (3) I Haskell er et regulært uttrykk representert ved hjelp av typen `Regex` fra `Text.Regex.Posix` modulen.

# Se også

- https://en.wikipedia.org/wiki/Regular_expression
- https://hackage.haskell.org/package/regex-posix-0.95.2/docs/Text-Regex-Posix.html