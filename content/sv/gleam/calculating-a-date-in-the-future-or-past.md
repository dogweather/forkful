---
title:                "Beräkning av ett datum i framtiden eller det förflutna"
html_title:           "Gleam: Beräkning av ett datum i framtiden eller det förflutna"
simple_title:         "Beräkning av ett datum i framtiden eller det förflutna"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Varför

Vi har alla varit där - du behöver ett program som beräknar ett datum i framtiden eller förflutet, men det verkar alltid vara en massa knepiga matematik eller långsamma loops. Men med hjälp av Gleam, kan du göra dessa beräkningar enkelt och snabbt.

## Hur man gör

För att beräkna en datum i framtiden eller förflutet med Gleam, behöver du bara använda Date-modulen och dess `add` och `subtract` funktioner. Till exempel, för att få en dag som är 10 dagar senare än idag, kan du använda följande kod:

```Gleam
import gleam/date

let future_date =
  date.now()
  |> date.add(10, "days")
```

Du kan också beräkna en datum i förflutet genom att använda `subtract` funktionen. Till exempel, om du vill ha ett datum som är 5 år tidigare än idag, kan du använda följande kod:

```Gleam
import gleam/date

let past_date =
  date.now()
  |> date.subtract(5, "years")
```

Det är också möjligt att använda andra tidsenheter såsom månader, veckor, timmar eller minuter i `add` och `subtract` funktionerna.

## Deep Dive

Om du vill gå djupare in i hur beräkningar av datum fungerar i Gleam, kan du utforska Date-modulen i Gleams dokumentation. Du kan också läsa mer om olika tidsenheter och hur de påverkar beräkningarna.

## Se även

- Gleam Date-Modulen: https://gleam.run/modules/date.html
- Gleam Dokumentation: https://gleam.run/
- Gleam Community Forum: https://forum.gleam.run/