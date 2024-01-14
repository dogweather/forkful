---
title:    "Gleam: Beregning av dato i fremtid eller fortid"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Hvorfor
Vil du kunne planlegge fremtiden din ved å beregne datoer i fremtiden eller i fortiden? Eller lurer du på hvordan du kan få Gleam-programmet ditt til å gjøre det for deg? Denne bloggposten vil gi deg en enkel forklaring og noen eksempler på hvordan du kan gjøre det.

## Hvordan
For å beregne en dato i fremtiden eller fortiden i Gleam, kan du bruke funksjonen `Calendar.date_add`. Denne funksjonen tar inn en dato og et antall dager, uker, måneder eller år du vil legge til eller trekke fra. La oss si at vi ønsker å beregne datoen som er fem dager fra i dag:

```Gleam
let today = Calendar.today()
let future_date = Calendar.date_add(today, Calendar.Duration.Day(5))
```

Dette vil gi oss datoen som er fem dager etter `today`. Du kan også bruke negative tall for å beregne en dato i fortiden. Her er et eksempel hvor vi søker etter datoen som var to uker siden:

```Gleam
let today = Calendar.today()
let past_date = Calendar.date_add(today, Calendar.Duration.Week(-2))
```

For å få en bedre forståelse av hvordan denne funksjonen fungerer, kan du også prøve å bruke den med andre enheter som måneder og år.

## Dypdykk
For å beregne en mer nøyaktig dato, kan du også bruke `Calendar.date_add` med en tidsperiode i stedet for en bestemt dato. Dette er nyttig hvis du for eksempel ønsker å finne ut hvilken dag det vil være fem måneder fra nå. Du kan også bruke `Calendar.date_subtract` for å beregne datoen i fortiden ved hjelp av en tidsperiode.

Merk at disse funksjonene tar hensyn til skuddår og andre kalenderjusteringer, slik at du alltid får en nøyaktig dato.

## Se også
* [Offisiell Gleam-dokumentasjon for `Calendar`-modulen](https://gleam.run/built-in-types-and-modules/calendar)
* [Eksempler på funksjonskall for `Calendar.date_add`](https://github.com/gleam-lang/gleam/blob/v0.13.0/examples/date_add.gleam)
* [Eksempler på funksjonskall for `Calendar.date_subtract`](https://github.com/gleam-lang/gleam/blob/v0.13.0/examples/date_subtract.gleam)