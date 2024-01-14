---
title:                "Gleam: Beräkning av ett datum i framtiden eller det förflutna"
simple_title:         "Beräkning av ett datum i framtiden eller det förflutna"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Varför

Enkelte gånger behöver vi kunna beräkna datum i framtiden eller i det förflutna i vår programkod. Detta kan vara för att schemalägga händelser eller för att jämföra tidsintervall. Med hjälp av Gleam kan vi enkelt utföra dessa beräkningar.

## Hur man gör

För att beräkna ett datum i framtiden eller förflutna i Gleam, behöver vi använda funktionen `Date.add` i standardbiblioteket `gleam/time`.

För att beräkna ett datum i framtiden, behöver vi ange ett initialt datum och ett antal dagar som vi vill lägga till. Till exempel, om vi vill beräkna datumet fyra dagar efter 1:a januari 2020, kan vi använda följande kod:

```Gleam
import gleam/time.{Date}

let starting_date = Date.from_iso8601("2020-01-01")
let future_date = Date.add(starting_date, 4)
```

Detsamma gäller om vi vill beräkna ett datum i förflutna, men istället för att lägga till dagar så subtraherar vi dem. Till exempel, om vi vill beräkna datumet tre dagar innan 1:a januari 2020:

```Gleam
import gleam/time.{Date}

let starting_date = Date.from_iso8601("2020-01-01")
let past_date = Date.add(starting_date, -3)
```

Funktionen `Date.add` returnerar ett nytt `Date`-objekt med det beräknade datumet. Detta gör att vi enkelt kan använda det för att jämföra med andra datum eller utföra andra operationer.

## Djupdykning

För att förstå hur Gleam beräknar datum i förflutna eller framtiden, behöver vi veta hur `Date`-objektet representerar datum. Varje `Date` har en årtal, månad och dag attribut, som sedan omvandlas till det mest använda formatet ISO 8601. När vi använder `Date.add` funktionen, så adderar eller subtraherar vi bara det angivna antalet dagar från det givna datumet.

Det viktigaste att komma ihåg är att Gleam inte hanterar skottår eller andra komplexa kalenderregler. Om det är nödvändigt, så kan vi implementera vår egen logik för att hantera dessa speciella fall.

## Se även

- [Dokumentation för gleam/time](https://gleam.run/modules/gleam/time/latest/)
- [ISO 8601 standard](https://www.iso.org/iso-8601-date-and-time-format.html) för datum och tid representation