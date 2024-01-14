---
title:                "Gleam: Sammenligning av to datoer"
simple_title:         "Sammenligning av to datoer"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/comparing-two-dates.md"
---

{{< edit_this_page >}}

Gleam for å sammenligne to datoer

Å programmere handler ofte om å håndtere og manipulere data. I mange tilfeller er en av de viktigste dataene å håndtere datoer. Datoer blir brukt til å planlegge hendelser, spore tidsavhengige data og løse utfordringer som bare dukker opp på bestemte datoer. Derfor er det viktig å kunne sammenligne to datoer når man jobber med programmering. I denne bloggposten vil vi utforske hvordan du kan gjøre dette ved hjelp av Gleam.

## Hvorfor

Å sammenligne to datoer kan være nyttig for å bestemme om en dato kommer før eller etter en annen. Dette er spesielt nyttig for å filtrere data, planlegge oppgaver eller holde styr på betalingsfrister. Ved å sammenligne to datoer kan du automatisere disse oppgavene og spare deg for manuell beregning og feil.

## Hvordan

For å sammenligne to datoer i Gleam kan du bruke funksjonen `Date.compare`. Denne funksjonen tar inn to `Date`-argumenter og returnerer en `Order`-enumverdi som enten er `Less`, `Equal` eller `Greater`. La oss se på et eksmpel:

```Gleam
let start_date = Date.new(2021, 07, 15)
let end_date = Date.new(2021, 07, 20)

let comparison = Date.compare(start_date, end_date)
```

I dette eksempelet vil `comparison`-variabelen inneholde verdien `Less`, siden `start_date` kommer før `end_date`.

Du kan også sammenligne datoer basert på tidsavhengige parametere, som for eksempel måned eller år. Dette kan gjøres ved hjelp av funksjonene `Date.compare_month` og `Date.compare_year`. Disse funksjonene tar inn samme argumenter som `Date.compare` og returnerer også en `Order`-enumverdi.

## Dypdykk

Når du jobber med datoer kan du også støte på behovet for mer komplekse sammenligninger, som å sjekke om en dato ligger mellom to andre datoer eller om en dato er på en spesifikk ukedag. Dette kan gjøres ved hjelp av biblioteket `gleam_dates` som gir ekstra funksjoner for å håndtere datoer. Du kan enkelt legge til dette biblioteket i ditt prosjekt ved å inkludere følgende linje i `gleam.toml`-filen:

```
[dependencies]
gleam_dates = { git = "https://github.com/gleam-lang/gleam-dates.git", tag = "v0.4.0" }
```

For mer informasjon om hvordan du kan bruke dette biblioteket, kan du se på dokumentasjonen på [GitHub-siden](https://github.com/gleam-lang/gleam-dates).

## Se Også

* [Offisiell dokumentasjon for Gleam](https://gleam.run/)
* [Gleam Playground](https://play.gleam.run/)
* [Gleam-dates biblioteket på GitHub](https://github.com/gleam-lang/gleam-dates)