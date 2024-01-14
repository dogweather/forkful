---
title:                "Elixir: Beräkning av ett datum i framtiden eller förflutna"
programming_language: "Elixir"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Varför
Att kunna beräkna ett datum i framtiden eller det förflutna är en viktig funktion inom programmering. Det kan hjälpa till att skapa dynamiska applikationer och organisera data på ett mer effektivt sätt.

## Så här gör du
För att kunna beräkna ett datum i Elixir, behöver vi använda funktionen `Calendar.DateTime.add/4`. Detta låter oss lägga till en viss tidsperiod till ett befintligt datum och få tillbaka ett nytt datum.

```Elixir
Calendar.DateTime.add(DateTime.utc_now(), 1, :month) 
```
Detta exempel skulle lägga till en månad till dagens datum, försök att experimentera med andra värden som dagar, veckor eller år för att skapa olika resultat.

För att beräkna ett datum i det förflutna, behöver du använda ett negativt värde i funktionen. Till exempel, om vi ville få tillbaka datumet från 5 dagar sedan, skulle vi använda:

```Elixir
Calendar.DateTime.add(DateTime.utc_now(), -5, :day)
```

## Djupdykning
Förutom att kunna lägga till eller dra ifrån en viss tidsperiod, kan vi också använda oss av funktionen `Calendar.DateTime.diff/2`. Detta låter oss jämföra två datum och få tillbaka skillnaden i antal sekunder.

En annan funktion som kan vara användbar är `Calendar.DateTime.to_erl/1` som konverterar ett Elixir datum till Erlang format. Detta kan vara användbart om du arbetar med externa bibliotek eller API:er som använder Erlang format.

## Se också
- [Elixir dokumentation för DateTime](https://hexdocs.pm/elixir/Calendar.DateTime.html)
- [Elixir Forum tråd om att beräkna datum](https://elixirforum.com/t/calculating-specific-date-in-the-future/16146)
- [Elixir style guide för kodning av datum](https://github.com/lexmag/elixir-style-guide#date-and-time)