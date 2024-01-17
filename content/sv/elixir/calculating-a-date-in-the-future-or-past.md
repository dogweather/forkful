---
title:                "Att beräkna ett datum i framtiden eller i det förflutna"
html_title:           "Elixir: Att beräkna ett datum i framtiden eller i det förflutna"
simple_title:         "Att beräkna ett datum i framtiden eller i det förflutna"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elixir/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att beräkna ett datum i framtiden eller det förflutna är en vanlig uppgift inom programmering. Det kan användas för att skapa tidslinjer, scheman eller hantera olika tidszoner. Det kan även användas för att lösa problem med datum och tid i olika applikationer.

## Så här gör du:
```Elixir 
Date.add(Date.utc_today(), 7)
```
Resultatet av denna kod skulle vara ett datum som ligger en vecka framåt från dagens datum i UTC-tidzonen. Du kan också ange ett negativt tal för att få ett datum i det förflutna.

För att lägga till ett specifikt antal dagar, månader eller år kan du använda funktionen `Date.add` tillsammans med `:days`, `:months` eller `:years` som andra argument. Till exempel:
```Elixir 
Date.add(Date.utc_today(), 3, :months)
```
Detta skulle ge dig ett datum som ligger tre månader framåt från dagens datum i UTC-tidzonen.

## Djupdykning:
För att beräkna datum i framtiden eller det förflutna, är det viktigt att förstå hur datum hanteras i Elixir. Datum i Elixir representeras som en tuple med tre element: år, månad och dag. Detta gör det lätt att utföra beräkningar på datum och jämföra dem med varandra.

En alternativ metod för att beräkna ett datum i framtiden eller det förflutna är att använda funktionen `Date.add/2` och ange en ändring i form av en lista, till exempel `[{17, :days}]` för att lägga till 17 dagar till datumet.

Elixir har också funktionen `Date.diff/2` som kan användas för att beräkna antalet dagar mellan två datum. Detta kan vara användbart för att beräkna hur lång tid det är från ett datum till ett annat.

## Se även:
- [Elixir Date modulen](https://hexdocs.pm/elixir/Date.html)
- [Elixir Calendar module](https://hexdocs.pm/elixir/Calendar.html)
- [Elixir Datetime module](https://hexdocs.pm/elixir/DateTime.html)