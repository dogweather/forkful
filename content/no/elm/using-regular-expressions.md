---
title:    "Elm: Å bruke regulære uttrykk"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/elm/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Hvorfor

I mange programmeringsspråk, inkludert Elm, kan vi oppleve å måtte behandle tekststrenger for å gjøre spesielle operasjoner. En måte å effektivt håndtere slike strenger er ved hjelp av regulære uttrykk (regular expressions). Dette er mønstre som lar oss søke etter og manipulere tekststrenger basert på visse kriterier. Med dette verktøyet kan vi enkelt finne og erstatte deler av tekst, eller til og med validerere inndata fra brukere. Hvis du vil bli en mer effektiv Elm-programmerer, er det absolutt verdt å lære om regulære uttrykk.

## Hvordan

For å bruke regulære uttrykk i Elm, må vi først importere Regex-modulen:

```Elm
import Regex exposing (..)
```

Deretter kan vi bruke funksjoner som <code>find</code> og <code>replace</code> for å manipulere tekststrenger. La oss si at vi har en liste med e-postadresser og vi bare vil vise de som inneholder en bestemt domene:

```Elm
let
    emails =
        [ "test@test.com", "hello@world.com", "foo@bar.com" ]
    
    domain =
        "world.com"
    
    filteredEmails =
        List.filter (\email -> Regex.find (Regex.regex domain) email) emails
in
    filteredEmails

-- Output:
-- [ "hello@world.com" ]
```

Som du kan se, brukte vi <code>find</code> for å søke etter e-postadresser som inneholder "world.com". Det er også mulig å erstatte deler av tekststrenger med <code>replace</code> funksjonen. La oss si at vi ønsker å endre alle forekomster av "elm" til "Elm" i en tekststreng:

```Elm
let
    text =
        "dette er en tekst om elm-programmering"
in
    Regex.replace (Regex.regex "elm") (\_ -> "Elm") text

-- Output:
-- "dette er en tekst om Elm-programmering"
```

## Dypdykk

Hvis du vil lære mer om regulære uttrykk i Elm, kan du sjekke ut dokumentasjonen på Elm sine nettsider. Der finner du en komplett oversikt over alle funksjoner og deres bruksområder. Det er også mange nyttige ressurser på nettet, som eksempler og tutorials, som kan hjelpe deg å forstå og mestre regulære uttrykk.

## Se også

- [Elm sin dokumentasjon om regulære uttrykk](https://package.elm-lang.org/packages/elm/regex/latest/)
- [Tutorial om regulære uttrykk i Elm](https://www.elm-tutorial.org/en/05-advanced/01-regular-expressions.html)