---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
- 2024-02-05, dogweather, reviewed and corrected
date: 2024-02-03 19:09:27.907362-07:00
description: "\xC5 f\xE5 den gjeldende datoen i Elixir involverer \xE5 f\xE5 tilgang\
  \ til systemets dato- og tidsinformasjon, en vanlig oppgave for logging, datostempling,\
  \ eller\u2026"
lastmod: '2024-03-13T22:44:40.453955-06:00'
model: gpt-4-0125-preview
summary: "\xC5 f\xE5 den gjeldende datoen i Elixir involverer \xE5 f\xE5 tilgang til\
  \ systemets dato- og tidsinformasjon, en vanlig oppgave for logging, datostempling,\
  \ eller enhver funksjonalitet som krever kunnskap om den n\xE5v\xE6rende datoen."
title: "F\xE5 dagens dato"
weight: 29
---

## Hva & Hvorfor?
Å få den gjeldende datoen i Elixir involverer å få tilgang til systemets dato- og tidsinformasjon, en vanlig oppgave for logging, datostempling, eller enhver funksjonalitet som krever kunnskap om den nåværende datoen. Denne operasjonen er essensiell for å skape tidsbevisste applikasjoner og for oppgaver som å generere rapporter eller tidsstempler i en webapplikasjon.

## Hvordan:
Elixirs standardbibliotek, gjennom `DateTime`-modulen, tillater henting av gjeldende dato og tid. Siden Elixir kjører på Erlang VM (BEAM), utnytter den de underliggende Erlang-funksjonalitetene for tidsoperasjoner.

### Ved bruk av Elixirs standardbibliotek
Elixir tilbyr funksjonen `DateTime.utc_now/0` for å få gjeldende dato og tid i UTC.

```elixir
current_datetime_utc = DateTime.utc_now()
IO.inspect(current_datetime_utc)
```

**Eksempel på utdata:**
```
~U[2024-02-05 19:58:40.925931Z]
```

For å bare få den gjeldende datoen, kan du trekke ut komponentene for år, måned og dag:

```elixir
{:ok, current_date} = Date.new(current_datetime_utc.year, current_datetime_utc.month, current_datetime_utc.day)
IO.inspect(current_date)
```

**Eksempel på utdata:**
```
~D[2023-05-04]
```

### Ved bruk av Timex-biblioteket
For mer komplekse dato-klokkeslettbehov, kan et populært tredjepartsbibliotek kalt Timex brukes. Først, legg til `Timex` i dine mix.exs-avhengigheter:

```elixir
defp deps do
  [
    {:timex, "~> 3.7"}
  ]
end
```

Etter å ha installert avhengigheten (`mix deps.get`), kan du bruke Timex til å få den gjeldende datoen:

```elixir
current_date = Timex.today()
IO.inspect(current_date)
```

**Eksempel på utdata:**
```
~D[2023-05-04]
```

Timex tilbyr omfattende funksjonaliteter for manipulering av dato-klokkeslett, noe som gjør det til et kraftig tillegg til dine Elixir-applikasjoner, spesielt når du håndterer tidssoner, formatering og parsing av datoer og tider.

Ved å forstå og utnytte Elixirs innebygde kapasiteter og Timex-biblioteket, kan du enkelt jobbe med datoer og klokkeslett i Elixir-applikasjonene dine, tilpasse opplevelsen til behovene til applikasjonen din med presisjon og letthet.
