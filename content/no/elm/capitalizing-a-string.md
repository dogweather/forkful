---
title:    "Elm: Store bokstaver i en streng"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/elm/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

Å kunne kapitalisere en streng er en grunnleggende ferdighet som er viktig for programmerere å lære. Det vil tillate deg å formatere tekst på en mer lesbar og profesjonell måte.

## Slik gjør du det

For å kapitalisere en streng i Elm, kan du bruke funksjonen `String.toUpper`. Denne funksjonen tar en streng som argument og returnerer en ny streng med alle bokstavene i store bokstaver.

```elm
import String

capitalizedString = "hei på deg"

capitalizedString = String.toUpper "hei på deg"

--output: "HEI PÅ DEG"
```

En annen måte å kapitalisere en streng på er å bruke funksjonen `String.words` og deretter sette den første bokstaven i hvert ord til store bokstaver ved hjelp av `String.map` og `Char.toUpper`. Dette vil gi samme resultat som `toUpper` funksjonen, men kan være nyttig hvis du bare vil kapitalisere første bokstav i hvert ord.

```elm
import String
import Char

capitalizedString = "hei på deg"

capitalizedString = String.join " " (String.map (\word -> (String.fromChar (Char.toUpper <| String.head word)) ++ String.tail word) (String.words capitalizedString))

--output: "Hei På Deg"
```

## Dykk ned

Det er viktig å merke seg at `toUpper` funksjonen bare kapitaliserer engelske bokstaver. For å håndtere alle typer språk og tegnsett, kan du bruke funksjonen `String.toTitle` som tar hensyn til spesifikke regler for språket du arbeider med.

Det er også verdt å nevne at både `toUpper` og `toTitle` funksjonene er immutabel, noe som betyr at de ikke endrer den opprinnelige strengen, men i stedet returnerer en ny streng med den ønskede endringen. Dette er viktig å huske når du arbeider med strenger i Elm.

## Se også

- [Elm dokumentasjon om strenger](https://package.elm-lang.org/packages/elm/core/latest/String)
- [Tutorial om grunnleggende tekstformattering i Elm](https://dev.to/lfdebrux/formatting-text-in-elm-57h3)