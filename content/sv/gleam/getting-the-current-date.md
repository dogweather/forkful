---
title:                "Gleam: Att få den aktuella datumet"
simple_title:         "Att få den aktuella datumet"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Varför

Att kunna få den nuvarande datumet i ditt program kan vara användbart för att visa när en händelse inträffade, eller för att jämföra datum mellan olika delar av din kod.

## Så här gör du

För att få den nuvarande datumet i Gleam, kan du använda funktionen `Time.now()` tillsammans med modulen `Time.Date`. Här är ett exempel:

```Gleam
import Time.Date

let nuvarande_datum = Time.now() |> Time.Date.from_gregorian
```

Detta kommer att ge dig ett datumobjekt med information om dag, månad och år. Du kan sedan använda `Time.Date`-funktioner som `day_of_week` och `month_name` för att få specifik information från datumet.

## Deep Dive

För den som är intresserad av att veta mer om hur man får det nuvarande datumet i Gleam, så använder sig `Time.now()` av Erlangs inbyggda funktion `erlang:now()`, som returnerar den aktuella tidpunkten som antal sekunder sedan 1 januari 1970. Detta antal måste sedan omvandlas till ett Gleam-datumobjekt med hjälp av `Time.Date.from_gregorian`.

## Se även

- Gleams dokumentation för `Time.Date`-modulen: https://gleam.run/modules/time#date
- Mer information om Erlangs `erlang:now()`: https://erlang.org/doc/man/erlang.html#now-0