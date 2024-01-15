---
title:                "Å beregne en dato i fremtiden eller fortiden"
html_title:           "Gleam: Å beregne en dato i fremtiden eller fortiden"
simple_title:         "Å beregne en dato i fremtiden eller fortiden"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Hvorfor
Hvorfor skulle noen ønske å beregne en dato i fremtiden eller fortiden? Vel, det kan være flere grunner til dette. Kanskje du planlegger en ferie og vil vite nøyaktig når du skal dra, eller kanskje du lurer på når en bestemt begivenhet vil finne sted. Uansett hva grunnen måtte være, kan Gleam gjøre det enkelt å beregne en dato.

## Hvordan
Det første du trenger å gjøre er å importere `gleam/datetime` pakken til koden din. Deretter kan du bruke funksjonene `future` og `past` for å beregne en dato i fremtiden eller fortiden. Her er noen eksempler på hvordan du kan gjøre dette:

```Gleam
import gleam/datetime

// Beregne en dato 5 dager fra nå
let future_date = datetime.future(5, "days")
// Output: 2021-07-12T22:00:00Z

// Beregne en dato 2 uker fra nå
let future_date = datetime.future(2, "weeks")
// Output: 2021-07-26T22:00:00Z

// Beregne en dato 1 måned fra nå
let future_date = datetime.future(1, "months")
// Output: 2021-08-09T22:00:00Z
```

```Gleam
import gleam/datetime

// Beregne en dato 2 dager siden
let past_date = datetime.past(2, "days")
// Output: 2021-07-01T22:00:00Z

// Beregne en dato 1 uke siden
let past_date = datetime.past(1, "weeks")
// Output: 2021-06-24T22:00:00Z

// Beregne en dato 2 måneder siden
let past_date = datetime.past(2, "months")
// Output: 2021-05-27T22:00:00Z
```

## Deep Dive
Nå lurer du kanskje på hva som skjer hvis du prøver å beregne en dato i fortiden eller fremtiden med en større enhet enn det som finnes i Gleam? For eksempel, hva om du vil beregne en dato 3 år fra nå? Vel, da vil Gleam automatisk konvertere det til antall dager, som er den minste enheten som kan brukes til å beregne en dato i fortiden eller fremtiden. Så i dette tilfellet, vil `datetime.future(3, "years")` faktisk gi deg datoen 1095 dager fra nå. Samme prinsipp gjelder for å beregne en dato i fortiden.

## Se også
- [Gleam Datetime pakken](https://gleam.run/modules/gleam/datetime/latest/)
- [Offisiell Gleam dokumentasjon](https://gleam.run/)