---
title:                "Analysering av dato fra en streng."
html_title:           "Elm: Analysering av dato fra en streng."
simple_title:         "Analysering av dato fra en streng."
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elm/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

Hva & Hvorfor? 

Parsing av datoer fra en streng er en viktig oppgave for programmerere. Det betyr å konvertere en tekstbasert dato til et datofelt i programmet ditt. Dette er nyttig for å sortere og filtrere datoinformasjon, og for å gjøre beregninger som er avhengig av datoer.

Hvordan: 

```Elm

import Date exposing (Date, fromString)
import Maybe exposing (Just, Nothing)

dateString = "10/31/2021"

parsedDate = fromString dateString
--Output: Just (Date 2021 10 31)

invalidDateString = "Hello"
invalidDate = fromString invalidDateString
--Output: Nothing
```

Deep Dive: 

Parsing av datoer fra strenger har vært en viktig del av programmering helt siden computerens tidlige dager. Det er også flere måter å gjøre dette på, inkludert å bruke innebygde funksjoner i språket ditt eller å bruke tredjepartsbiblioteker. I Elm, kan du også bruke Date-modulen til å beregne tidsintervaller, konvertere datoer mellom ulike format og mye mer. Implementeringen av denne funksjonen gjøres ved å bruke en kombinasjon av regulære uttrykk og kontrollstrukturer.

Se også: 

- Elm Date-modulen dokumentasjon: https://package.elm-lang.org/packages/elm/time/latest/Date#Date
- Date parsing i JavaScript: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/parse