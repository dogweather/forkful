---
title:                "Søke og erstatte tekst"
html_title:           "Elm: Søke og erstatte tekst"
simple_title:         "Søke og erstatte tekst"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elm/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å søke og erstatte tekst er en vanlig oppgave for programmerere. Dette refererer til å finne spesifikke deler av en tekst og erstatte dem med annen tekst. Det kan være nyttig for å gjøre masseendringer, som å endre navn på variabler eller rette opp feil i koden.

## Hvordan:
Elm har en innebygd funksjon for å søke og erstatte tekst, kalt `String.replace`. Den tar inn to strenger, den første som er teksten du vil søke etter, og den andre som er teksten du vil erstatte den med. Her er et eksempel:

```Elm
import String exposing (replace)

tekst = "Hei, verden!"

modifisertTekst = replace "verden" "alle sammen" tekst

-- modifisertTekst = "Hei, alle sammen!"
```

## Dypdykk:
Søke og erstatte tekst er en vanlig funksjon i tekstbehandlingsprogrammer og teksteditorer. Det er også tilgjengelig i de fleste programmeringsspråk, inkludert Elm. Alternativene for å utføre dette i Elm inkluderer å bruke en annen funksjon, `String.replaceSub`, som lar deg gi et intervall av tekst som skal erstattes, og `Regex.replace`, som bruker regulære uttrykk for å finne teksten som skal erstattes.

Når det gjelder implementasjon i Elm, bruker `String.replace` funksjoner fra standardbiblioteket for å manipulere tekststrenger og utføre søk og erstatningsoperasjoner.

## Se også:
- Elm's String library documentation: https://package.elm-lang.org/packages/elm/core/latest/String
- Regular expressions in Elm: https://elmprogramming.com/regular-expressions-in-elm.html