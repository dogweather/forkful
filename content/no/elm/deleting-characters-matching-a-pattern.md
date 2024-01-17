---
title:                "Sletting av tegn som samsvarer med et mønster"
html_title:           "Elm: Sletting av tegn som samsvarer med et mønster"
simple_title:         "Sletting av tegn som samsvarer med et mønster"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elm/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å slette tegn som matcher et mønster er en vanlig oppgave for programmere. Dette innebærer å fjerne spesifikke tegn eller tegnkombinasjoner fra en streng, basert på et forhåndsdefinert mønster. Dette kan være nyttig for å rydde opp i data, filtrere ut uønskede tegn eller formatere tekster på en mer konsistent måte.

## Hvordan:
```Elm
import String

-- Sletter alle tegn i en streng som matcher mønsteret [A-Z]

deleteCharacters: String -> String
deleteCharacters str =
  String.filter (\c -> c `List.member` ['A'..'Z']) str

-- Input: "Here Are Some Numbers: ABC123"
-- Output: "re re me  umers: 123"
```

Det er flere måter å implementere denne funksjonen på, avhengig av utfordringen du står overfor og språket du bruker. Ved hjelp av Elm sin innebygde String modul kan du filtrere ut uønskede tegn med `String.filter` og `List.member` funksjonene.

## Dypdykk:
Å slette tegn som matcher et mønster er ikke en ny oppgave i programmering og har blitt løst på ulike måter gjennom årene. I tillegg til å bruke filter funksjoner, kan du også bruke regex (regular expressions) for mer avanserte og komplekse mønstre. Det finnes også andre funksjoner i Elm sin String modul som kan være nyttig, som for eksempel `String.replace` for å erstatte tegn som matcher et mønster.

## Se også:
- [Elm String modul dokumentasjon](https://package.elm-lang.org/packages/elm/core/latest/String)
- [Regex i Elm](https://guide.elm-lang.org/interop/regex.html)
- [Andre løsninger på å slette tegn i programmeringsspråk som Java og Python](https://www.geeksforgeeks.org/remove-all-occurrences-of-a-character-from-a-string-in-python/)