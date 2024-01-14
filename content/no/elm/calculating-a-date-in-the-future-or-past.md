---
title:    "Elm: Kalkulering av dato i fremtiden eller fortiden"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/elm/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Hvorfor
Hvorfor beregne en dato i fremtiden eller fortiden? Dette er en viktig funksjon i mange programmeringsspråk, inkludert Elm, fordi det lar oss håndtere datoer og tider på en enkel og nøyaktig måte. Enten du lager en kalenderapp, en bookingside eller en vaksineplanlegger, kan du bruke funksjoner for å beregne datoer for å gjøre programmene dine mer fleksible og nyttige.

## Hvordan
For å beregne en dato i fremtiden eller fortiden i Elm, kan du bruke den innebygde funksjonen `Date.fomNow` eller `Date.fromYMD`. Først må du importere `Date`-modulen ved å skrive `import Date` øverst i filen din. Deretter kan du bruke en av disse funksjonene til å opprette en dato- eller tidstype som du kan jobbe med.

```
Elm 0.19.1

import Date

Date.fromNow 8 Days
-- Oppretter en `Date`-type med datoen 8 dager fra nå

Date.fromYMD 2021 12 25
-- Oppretter en `Date`-type med datoen 25. desember 2021
```

For å få en utskrift av datoen, kan du bruke `Date.format`-funksjonen og spesifisere hvilket format du ønsker å bruke.

```
Elm 0.19.1

import Date

date = Date.fromNow 8 Days
-- Oppretter en `Date`-type med datoen 8 dager fra nå

Date.format "%Y %m %d" date
-- Returnerer "2021 08 30"
```

Du kan også bruke andre funksjoner som `Date.add`, `Date.sub` og `Date.compare` for å manipulere og sammenligne datoer.

## Dypdykk
Når det gjelder å beregne datoer, er det viktig å huske på at Elm bruker UTC-tid og at datoer bare kan være gyldige ifølge ISO-8601-standarden. Dette betyr at du må være nøyaktig når du oppretter datotyper og være forsiktig når du manipulerer dem. Du kan også bruke `Date.fromCalendarDate`-funksjonen for å håndtere ulike kalenderformater.

## Se Også
- Offisiell Elm dokumentasjon for `Date`-modulen: https://package.elm-lang.org/packages/elm/core/latest/Date
- Informasjon om ISO-8601-standarden: https://en.wikipedia.org/wiki/ISO_8601