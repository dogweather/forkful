---
title:                "Beräkning av ett datum i framtiden eller i det förflutna"
html_title:           "Elixir: Beräkning av ett datum i framtiden eller i det förflutna"
simple_title:         "Beräkning av ett datum i framtiden eller i det förflutna"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Varför

Att kunna beräkna ett datum i framtiden eller det förflutna är en viktig del av programmering. Det kan vara användbart för att skapa funktioner som hanterar tidsberäkningar eller för att visualisera tidsbaserade data.

## Så här gör du

```Elixir
iex> Date.add(~D[2020-01-01], 10)
~D[2020-01-11]

iex> Date.subtract(~D[2020-01-01], 5)
~D[2019-12-27]
```

För att beräkna ett datum i framtiden kan du använda funktionen `Date.add` och för att beräkna datumet i det förflutna kan du använda funktionen `Date.subtract`. Båda funktionerna tar ett datumelement (~D) som första argument och antalet dagar som ska läggas till eller dras bort som andra argument. Resultatet är ett nytt datumelement som returneras.

## Djupdykning

Vad som händer bakom kulisserna när du använder funktionerna `Date.add` och `Date.subtract` är att datumelementet omvandlas till en tupel ([år, månad, dag]) och sedan adderas eller subtraheras med det angivna antalet dagar. Slutligen omvandlas den nya tupeln tillbaka till ett datumelement och returneras.

Det är viktigt att notera att dessa funktioner inte ändrar det ursprungliga datumelementet utan returnerar ett nytt datumelement. Om du vill ändra det ursprungliga datumelementet måste du använda funktionerna `Date.add!` eller `Date.subtract!` som använder samma logik men ändrar det ursprungliga datumelementet.

## Se också

- Elixir Date modulens dokumentation: https://hexdocs.pm/elixir/Date.html 
- En tutorial för att arbeta med datum i Elixir: https://elixirschool.com/en/lessons/specifics/dates/