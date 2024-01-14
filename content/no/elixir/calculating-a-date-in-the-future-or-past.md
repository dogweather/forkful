---
title:    "Elixir: Beregning av en dato i fremtiden eller fortiden"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Hvorfor
Å beregne datoer i fremtiden eller fortiden er en vanlig oppgave for utviklere. Dette kan være nyttig for å lage tidssensitive applikasjoner eller for å automatisere oppgaver som må gjøres på bestemte datoer. I denne blogginnlegget vil vi se på hvordan man kan gjøre dette med Elixir programmeringsspråket.

## Hvordan
For å beregne en dato i fremtiden eller fortiden i Elixir, kan du bruke funksjonen `Date.add/2`. Denne funksjonen tar inn en dato og et heltall som representerer antall dager som skal legges til eller trekkes fra datoen. La oss se på et eksempel:

```Elixir
iex> Date.add(Date.utc_today(), 7)
{:ok, ~U[2019-06-18]}
```

Her har vi brukt funksjonen `Date.utc_today()` for å få dagens dato og deretter lagt til 7 dager ved hjelp av `Date.add/2` funksjonen. Dette returnerer en tuple med `:ok` og den nye datoen lagret som et `Date` objekt.

For å beregne en dato i fortiden, kan du bruke et negativt tall som antall dager. La oss si at vi ønsket å finne datoen som var en uke siden fra i dag:

```Elixir
iex> Date.add(Date.utc_today(), -7)
{:ok, ~U[2019-06-04]}
```

Som du kan se, returnerer funksjonen den korrekte datoen, 7 dager før dagens dato.

## Deep Dive
Elixir har en rekke funksjoner for å håndtere datoer og tider. For å lære mer om disse, sjekk ut dokumentasjonen for `Date` og `DateTime` modulene. I tillegg kan du bruke funksjoner som `Date.is_leap_year/1` for å sjekke om et bestemt år er et skuddår, eller `Date.day_of_week/1` for å få ukedagen for en gitt dato.

For mer avanserte beregninger og manipulasjoner av dato og tid, kan du bruke Elixir biblioteker som `Timex` eller `Calendar`. Disse gir deg flere funksjoner og metoder for å håndtere dato og tid på en enklere måte.

## Se også
- Elixir offisiell dokumentasjon for `Date` og `DateTime` moduler: https://hexdocs.pm/elixir/Date.html, https://hexdocs.pm/elixir/DateTime.html
- Timex biblioteket: https://github.com/bitwalker/timex
- Calendar biblioteket: https://github.com/lau/calendar