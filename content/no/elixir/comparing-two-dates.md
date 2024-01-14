---
title:                "Elixir: Sammenligning av to datoer"
simple_title:         "Sammenligning av to datoer"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elixir/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Hvorfor
Hvis du noen gang har programmert i Elixir, har du sannsynligvis støtt på situasjoner der du trenger å sammenligne to datoer. Dette kan være for å filtrere data, sortere en liste eller utføre andre operasjoner. I denne bloggen vil vi utforske hvordan man kan sammenligne to datoer i Elixir, og hvorfor det kan være nyttig.

## Hvordan gjøre det
Det er flere måter å sammenligne to datoer på i Elixir, avhengig av hvilket format du har dem i. La oss se på noen eksempler ved hjelp av den innebygde `Date`-modulen.

For å starte kan vi opprette to datoer ved å bruke funksjonen `Date.new(year, month, day)`. La oss si at vi ønsker å sammenligne to datoer: 1. januar 2020 og 15. februar 2020.

```Elixir
date1 = Date.new(2020, 1, 1)
date2 = Date.new(2020, 2, 15)
```

For å sammenligne disse to datoene kan vi bruke operatorer som `==`, `<=` og `>=`.

```Elixir
date1 == date2 # => false
date1 <= date2 # => true
date1 >= date2 # => false
```

Vi kan også bruke funksjonen `Date.compare/2` som returnerer -1, 0 eller 1 avhengig av om den første datoen er før, på samme dag eller etter den andre datoen.

```Elixir
Date.compare(date1, date2) # => -1
```

Hvis du har datoer i forskjellige formater, for eksempel en streng og en `DateTime`-instans, kan du konvertere den ene til det andre formatet ved hjelp av funksjoner som `Date.from_iso8601/1` og `Date.to_datetime/1` før du sammenligner dem på samme måte.

## Dypdykk
En ting å merke seg er at når du sammenligner to datoer, sammenlignes de ikke bare som strenger eller som tall, men snarere som datoobjekter. Dette betyr at hvis du sammenligner to datoer i forskjellige tidszoner eller med forskjellig nøyaktighet (år, måned, dag), kan du få uventede resultater. Det er derfor viktig å være klar over dette når du sammenligner datoer i Elixir.

En annen ting å merke seg er at `Date`-modulen også har funksjoner som `Date.add/2` og `Date.diff/2` som kan være nyttige hvis du trenger å legge til eller trekke fra dager fra en dato.

## Se også
Se gjerne på disse ressursene for mer informasjon om å sammenligne datoer i Elixir:

- [Offisiell Elixir `Date`-modul dokumentasjon](https://hexdocs.pm/elixir/Date.html)
- [ElixirSchool: Datoer og tid](https://elixirschool.com/no/lessons/basics/dates-and-times/)
- [ElixirForum: Sammenligne datoer](https://elixirforum.com/t/comparing-dates/12417)