---
title:                "Beräkna ett datum i framtiden eller det förflutna"
html_title:           "Gleam: Beräkna ett datum i framtiden eller det förflutna"
simple_title:         "Beräkna ett datum i framtiden eller det förflutna"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Vad och varför?
 
Att beräkna ett datum i framtiden eller förflutna innebär att bestämma en specifik dag i förhållande till en given startpunkt. Detta kan vara användbart för programmerare när de behöver skapa tidsbaserade funktioner och applikationer. Genom att kunna beräkna datumet för framtida eller förflutna händelser, kan vi enkelt planera och organisera våra program.

## Så här gör du:

``` gleam 
import Date

let start_date = Date.local(2020, 10, 15)
let future_date = Date.add_days(start_date, 7)

let past_date = Date.sub_days(start_date, 5)

IO.display("Startdatum: ", start_date)
IO.display("Datum i framtiden: ", future_date)
IO.display("Datum i förflutna: ", past_date)
```

### Utdata:

```2020-10-15T00:00:00.000```
```2020-10-22T00:00:00.000```
```2020-10-10T00:00:00.000```

## Djupdykning:

### Historisk bakgrund:

Beräkning av datum i framtiden eller förflutet har varit en viktig del av programmering sedan lång tid tillbaka. Detta är särskilt viktigt för tidskritiska system som behöver hantera tidszoner och olika kalendersystem.

### Alternativ:

Det finns många olika programmeringsspråk som erbjuder funktioner för att beräkna datum i framtiden eller förflutna. Några andra populära alternativ inkluderar Ruby och Python.

### Implementation detaljer:

Gleam använder inbyggda funktioner i DateTime-modulen för att göra dessa beräkningar. Funktioner som `add_days` och `sub_days` gör det möjligt att enkelt lägga till eller subtrahera antal dagar från ett givet datum.

## Se även:

- [Gleam DateTime Modul](https://gleam.run/modules/gleam/datetime/latest/DateTime.html)
- [Ruby DateTime klass](https://ruby-doc.org/stdlib-2.5.3/libdoc/date/rdoc/DateTime.html)
- [Python datetime modul](https://docs.python.org/2/library/datetime.html)