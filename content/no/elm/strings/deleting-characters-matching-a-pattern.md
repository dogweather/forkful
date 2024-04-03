---
date: 2024-01-20 17:41:58.077726-07:00
description: "How to: Elm har ikke innebygd regex, s\xE5 vi bruker `String` funksjoner\
  \ for \xE5 fjerne spesifikke tegn."
lastmod: '2024-03-13T22:44:40.692684-06:00'
model: gpt-4-1106-preview
summary: "Elm har ikke innebygd regex, s\xE5 vi bruker `String` funksjoner for \xE5\
  \ fjerne spesifikke tegn."
title: "Slette tegn som matcher et m\xF8nster"
weight: 5
---

## How to:
Elm har ikke innebygd regex, så vi bruker `String` funksjoner for å fjerne spesifikke tegn.

```Elm
import String

removeVowels : String -> String
removeVowels str =
    String.filter (\char -> not (char `elem` "aeiouAEIOU")) str

-- Bruk:
result = removeVowels "Hello, Elm Programmer!"
-- result == "Hll, Elm Prgrmmr!"
```

Output vil være strengen med vokaler fjernet.

## Deep Dive
Elm fokuserer på enkelhet og pålitelighet, så det har ikke med komplekse funksjoner som regex som standard. Historisk sett er dette valget for å unngå de kompleksitetene og potensielle feilene som følger med regex. Istedenfor, bruk `String` funksjoner for å skape custom filtreringslogikk som vist ovenfor. Alternativer inkluderer å lage en Elm Native Module eller bruke ports for å håndtere regex i JavaScript.

## See Also
- Elm String documentation: https://package.elm-lang.org/packages/elm/core/latest/String
- Elm community discussions on string manipulation: https://discourse.elm-lang.org/
- Ports in Elm for advanced manipulation: https://guide.elm-lang.org/interop/ports.html
