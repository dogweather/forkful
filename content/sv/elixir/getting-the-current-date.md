---
title:                "Elixir: Att få aktuellt datum."
programming_language: "Elixir"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Varför
Att kunna hämta den aktuella datumet är en viktig funktion i många program och applikationer. Det kan användas för att spåra tidsbaserade händelser, generera tidsstämplar och mer.

## Hur man gör det
Det är enkelt att få det nuvarande datumet i Elixir med hjälp av ```Date.utc_today/0``` funktionen. Se nedan för ett exempel:

```Elixir
iex> Date.utc_today()
#Elixir.Date<2021-04-15>
```

Som standard är resultatet formaterat som ett ```Date``` objekt, men det kan också enkelt formateras enligt användarens preferenser med hjälp av ```Date.to_string/1``` funktionen. Till exempel:

```Elixir
iex> Date.to_string(Date.utc_today(), {:short})
2021-04-15
iex> Date.to_string(Date.utc_today(), {:day, [locale: :sv]})
4 april, 2021
```

Det finns också möjlighet att få det lokala datumet, som är baserat på användarens tidszon, genom att använda ```Date.local_today/0``` funktionen.

## Djupdykning
Det finns flera andra funktioner som kan användas för att hantera datum i Elixir, inklusive ```Date.from_erl/1``` för att konvertera från Erlang datumformat, ```Date.compare/2``` för att jämföra datum och ```Date.add/3``` för att lägga till eller ta bort datum.

Ett annat användbart verktyg är ```Calendar``` modulen som innehåller funktioner för att arbeta med tider och datum, inklusive att omvandla mellan olika tidszoner.

## Se även
- [Elixir Date modul](https://hexdocs.pm/elixir/Date.html)
- [Elixir Calendar modul](https://hexdocs.pm/elixir/Calendar.html)
- [Datum och tidshantering i Elixir med date, time och timex bibliotek](https://dev.to/rakeshchechi170960/datetime-manipulation-in-elixir-using-date-time-and-timex-library-28ho)