---
title:                "Elixir: Sammenligning av to datoer"
programming_language: "Elixir"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elixir/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Hvorfor

Å sammenligne datoer er en vanlig oppgave innen programmering, spesielt når man arbeider med tidsstyring og planlegging. Det kan også være nyttig for å sortere og filtrere data basert på datoer, noe som er viktig for mange applikasjoner.

## Hvordan

Det er flere måter å sammenligne to datoer på i Elixir, avhengig av hva slags informasjon man vil ha ut av sammenligningen. Her er noen eksempler:

```Elixir
# Sjekke om datoer er like
"2019-08-03" == "2019-08-03"
=> true

# Sjekke om en dato er før en annen
"2019-08-03" < "2019-08-05"
=> true

# Få differansen i dager mellom to datoer
"2020-01-01" - "2019-12-31"
=> 1

# Sjekke om en dato er i en bestemt måned
"2020-01-01" |> Calendar.DateTime.to_date |> Date.month == 1
=> true

# Sjekke om en dato er i et bestemt år
"2020-01-01" |> Calendar.DateTime.to_date |> Date.year == 2020
=> true
```

Som du kan se, kan vi bruke vanlige sammenligningsoperatorer som `==` og `<` for å sammenligne to datoer direkte. Vi kan også konvertere datoer til forskjellige format og bruke funksjoner for å sjekke spesifikke egenskaper, som måned og år.

## Dypdykk

Når man sammenligner to datoer, er det viktig å være klar over forskjellige datoformater og hvordan de kan påvirke sammenligningen. For eksempel, hvis man sammenligner en dato med et klokkeslett, vil det kun sammenligne datoene og ikke ta hensyn til klokkeslettet. Det kan også være forskjeller i måten datoer vises på i forskjellige land og regioner.

En annen viktig ting å huske på er hvordan man håndterer skuddår og klokken som justeres for året rundt.

## Se også

- [Elixir DateTime dokumentasjon](https://hexdocs.pm/elixir/Calendar.DateTime.html)
- [Elixir Date dokumentasjon](https://hexdocs.pm/elixir/Date.html)
- [Elixir DateRange modul](https://hexdocs.pm/elixir/DateRange.html)
- [Elixir Task modul](https://hexdocs.pm/elixir/Task.html) (for asynkron sammenligning av datoer)