---
title:                "Elixir: Beregning av en dato i fremtiden eller fortiden"
simple_title:         "Beregning av en dato i fremtiden eller fortiden"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elixir/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Hvorfor

Å kunne beregne og manipulere datoer er en viktig del av enhver programmeringsoppgave som involverer tidsstyring. Enten det er å planlegge fremtidige hendelser eller å håndtere historiske data, er det nødvendig å kunne håndtere datoer riktig. I denne bloggposten vil vi se nærmere på hvordan man kan beregne datoer i fremtiden eller fortiden ved hjelp av Elixir.

# Hvordan

For å beregne en dato i fremtiden eller fortiden i Elixir, kan vi bruke funksjonen `DateTime.add/4`. Denne funksjonen tar inn fire argumenter - en dato, en tidsenhet, en verdi og en tidsstempelstype. La oss si at vi ønsker å beregne datoen 1 år fra i dag. Da kan vi bruke følgende kode:

```
DateTime.add(DateTime.utc_now(), :years, 1, :utc)
# => ~U[2022-04-15 18:29:47]
```

Her bruker vi funksjonen `DateTime.utc_now()` for å hente dagens dato og klokkeslett. Deretter spesifiserer vi at vi ønsker å legge til ett år, ved å bruke `:years` som argument. Til slutt setter vi også en tidsstempeltype, som i dette tilfellet er `:utc`. Dette vil gi oss datoen 1 år fra i dag i UTC-format.

Vi kan også bruke negative verdier for å beregne en dato i fortiden. For eksempel, hvis vi ønsker å finne datoen for 1 måned siden, kan vi bruke følgende kode:

```
DateTime.add(DateTime.utc_now(), :months, -1, :utc)
# => ~U[2021-03-15 18:29:47]
```

Som du kanskje merker, er det også mulig å spesifisere andre tidsenheter slik som `:days`, `:hours` og `:minutes`.

# Dykk dypere

For å forstå hvordan funksjonen `DateTime.add/4` egentlig fungerer, må vi vite litt om hvordan Elixir håndterer datoer og tider. I motsetning til enkelte andre programmeringsspråk, har Elixir ikke en egen datatype for datoer og tider. I stedet bruker det `DateTime`-modulen som en type wrapper rundt Erlangs `:calendar`-modul.

Dette betyr at når vi bruker funksjonen `DateTime.add/4`, så sender vi faktisk en `DateTime`-struktur til `:calendar`-modulen for å utføre beregningen. Dette er grunnen til at vi må spesifisere en tidsstempeltype - for å fortelle `:calendar` hvilken type tidsstempel vi forventer å få tilbake.

Det er også viktig å merke seg at Elixir håndterer alle datoer og tider som UTC, uavhengig av hvilken tidssone du befinner deg i. Dette er fordi UTC er en etablert standard for å håndtere tidssoner og DST (Daylight Saving Time).

# Se også

- [Elixir - DateTime-modulen](https://hexdocs.pm/elixir/DateTime.html)
- [Erlang - calendar-modulen](http://erlang.org/doc/man/calendar.html)
- [ISO 8601 - standard for dato- og tidsformater](https://www.iso.org/iso-8601-date-and-time-format.html)