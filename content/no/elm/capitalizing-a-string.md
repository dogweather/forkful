---
title:                "Sette streng til store bokstaver"
html_title:           "Arduino: Sette streng til store bokstaver"
simple_title:         "Sette streng til store bokstaver"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elm/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Kapitalisering av en streng betyr å gjøre første bokstav i hvert ord stort, ofte for å fremheve titler eller navn. Programmerere gjør dette for å forbedre lesbarheten eller tilpasse tekst til bestemte stilkrav.

## Hvordan:
Elm har ikke en innebygd funksjon for å kapitalisere hele strenger, men vi kan lage en selv. Her er et lite eksempel:

```Elm
import String exposing (toUpper, toLower, words, unwords)
import List exposing (map, head, tail)

capitalizeWord : String -> String
capitalizeWord word =
    case head word of
        Nothing ->
            ""
        
        Just firstChar ->
            String.cons (toUpper firstChar) (toLower (String.dropLeft 1 word))

capitalize : String -> String
capitalize =
    words >> map capitalizeWord >> unwords

-- Bruk slik:
main =
    String.toText (capitalize "elm er fantastisk!")
```

Forventet output:

```
"Elm Er Fantastisk!"
```

## Dypdykk
Etter å ha dukket litt rundt, finner vi ingen innebygd `capitalize` funksjon i Elm, i motsetning til noen andre språk. I eldre språk som C, håndteres dette ofte ved lavnivå-manipulasjon av ASCII-verdier. I moderne språk som JavaScript, finnes innebygde metoder som `.toUpperCase()`.

Alternativer i Elm inkluderer å lage egne funksjoner, som vist ovenfor, eller å bruke pakker som `elm-string-extra` som inkluderer flere nyttige strengoperasjoner.

Implementasjonsdetaljer bør vurdere locale. Visse språk har forskjellig regler for stor bokstav. Elm håndterer ikke locale-spesifikk logikk innbygd, så dette må gjøres manuelt om nødvendig.

## Se Også
For å utvide din Elm-strengbehandling, sjekk ut:
- Elm docs for `String`-modulen: https://package.elm-lang.org/packages/elm/core/latest/String
- `elm-string-extra` for flere strengfunksjoner: https://package.elm-lang.org/packages/elm-community/string-extra/latest/
- W3C sin anbefaling om språk-spesifikk typografi: https://www.w3.org/TR/i18n-html-tech-lang/