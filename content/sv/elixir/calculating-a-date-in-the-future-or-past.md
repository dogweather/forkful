---
title:                "Elixir: Beräkna ett datum i framtiden eller det förflutna"
simple_title:         "Beräkna ett datum i framtiden eller det förflutna"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Varför

Att kunna beräkna en datum i framtiden eller förflutna kan vara en användbar funktion i många programmeringsprojekt. Det finns många olika sätt att göra detta, men i denna bloggpost kommer vi att fokusera på hur man gör det i Elixir.

## Hur man gör det

För att beräkna ett datum i framtiden eller förflutna i Elixir, behöver vi använda oss av modulen `Date`. Detta är en inbyggd modul i Elixir som ger oss funktioner för hantering av datum. 

Först och främst behöver vi skapa ett datumobjekt som vi kan arbeta med. Detta görs genom att använda funktionen `Date.new/3` och ange året, månaden och dagen som argument. Ett exempel på hur man kan göra detta ser ut så här:

```
Elixir
date = Date.new(2020, 4, 1)
```

För att sedan beräkna ett datum i framtiden eller förflutna utgår vi från vårt initiala datumobjekt. Vi kan då använda funktionen `Date.add/4` för att lägga till eller dra bort dagar, månader eller år. Exempelvis om vi vill beräkna ett datum 5 dagar framåt ser det ut så här:

```
Elixir
new_date = Date.add(date, 5, :days)
```

Outputen av `new_date` skulle då vara `2020-04-06`. På samma sätt kan vi också dra bort dagar, månader eller år genom att ange ett negativt tal som andra argumentet för `Date.add/4`.

## Fördjupning

Nu när vi vet hur man beräknar ett datum i framtiden eller förflutna i Elixir, låt oss titta på några vanliga användningsområden och hur vi kan hantera dem.

**Beräkna en födelsedag**

En vanlig användning av att beräkna ett datum i förflutna är för att bestämma en persons födelsedag baserat på deras ålder. Detta kan göras genom att först skapa ett datumobjekt för deras födelsedag och sedan använda `Date.subtract/3` för att dra bort deras ålder från detta datum. Exempelvis:

```
Elixir
birth_date = Date.new(1990, 10, 15)
today = Date.today()

age = Date.diff(today, birth_date) |> elem(0)

calc_birthday = Date.subtract(birth_date, age, :years)
```

**Beräkna en förfallodatum för en faktura**

Ett annat vanligt användningsområde kan vara att beräkna ett förfallodatum för en faktura. Detta kan göras genom att först skapa ett datumobjekt för fakturadatumet och sedan använda `Date.add/4` för att lägga till antalet dagar som fakturan ska betalas inom. Exempelvis:

```
Elixir
invoice_date = Date.new(2020, 4, 10)

due_date = Date.add(invoice_date, 30, :days)
```

## Se även

Här är några användbara länkar för att lära dig mer om att arbeta med datum i Elixir:

- [Dokumentation för Date-modul](https://hexdocs.pm/elixir/Date.html)
- [Elixir School Guide: Datum och tid](https://elixirschool.com/sv/lessons/basics/dates-and-times/)
- [Date and Time i Elixir av ElixirForum](https://elixirforum.com/t/date-and-time-in-elixir/1151)