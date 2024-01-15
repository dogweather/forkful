---
title:                "Hente nåværende dato"
html_title:           "Elm: Hente nåværende dato"
simple_title:         "Hente nåværende dato"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elm/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Hvorfor

Å få den nåværende datoen er en enkel, men nyttig funksjon i mange programmer. Enten det er for å vise brukeren den aktuelle datoen, registrere når noe skjedde, eller for å planlegge fremtidige oppgaver, så er det å kunne få tak i dagens dato en viktig del av mange programmer.

## Hvordan

For å få den nåværende datoen i Elm, kan du bruke funksjonen ```Date.toIsoString```. Den returnerer dagens dato i ISO-format, slik som dette:

```Elm
Date.toIsoString Date.today
```

Output vil være noe som dette: 

```Elm
"2021-09-28"
```

Du kan også formatere datoen på en mer lesbar måte ved å bruke funksjonen ```Date.format``` og spesifisere et format du ønsker. For eksempel, for å få datoen i formatet "dd. MMM yyyy", kan du skrive:

```Elm
Date.format "%d. %b %Y" Date.today
```

Output vil da være noe som dette:

```Elm
"28. Sep 2021"
```

Du kan også få informasjon om tidspunktet for den nåværende datoen ved å bruke funksjonene ```Date.hour```, ```Date.minute```, og ```Date.second```. Disse vil returnere tall som representerer timen, minuttet og sekundet for dagens dato.

## Dypdykk

I tillegg til de nevnte funksjonene, kan du også få mer detaljert informasjon om den nåværende datoen ved å bruke funksjonen ```Date.toTime``` og deretter bruke funksjoner som ```Time.hour```, ```Time.minute``` og ```Time.second```. Dette vil gi deg tidspunktet for dagens dato i enklere å jobbe med format.

Det kan også være nyttig å vite hvordan man kan få tak i datoen for et bestemt tidspunkt, eller å legge til eller trekke fra et visst antall dager fra dagens dato. Dette kan enkelt gjøres ved å bruke funksjonene ```Date.fromIsoString``` og ```Date.fuse``` sammen med ulike datorelaterte funksjoner.

## Se også

- Offisiell Elm dokumentasjon om [Date](https://package.elm-lang.org/packages/elm-lang/core/latest/Date)
- Elm [Time package](https://package.elm-lang.org/packages/elm/time/latest/Time) for mer avanserte tidsfunksjoner.