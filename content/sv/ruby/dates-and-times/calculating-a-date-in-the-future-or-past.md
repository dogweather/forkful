---
title:                "Beräkna ett datum i framtiden eller förflutenheten"
date:                  2024-01-20T17:31:44.821523-07:00
model:                 gpt-4-1106-preview
simple_title:         "Beräkna ett datum i framtiden eller förflutenheten"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att räkna ut ett framtida eller förflutet datum innebär att lägga till eller dra ifrån dagar till en given dag. Programmerare gör detta för att hantera bokningar, påminnelser eller tidsbaserade funktioner i sina applikationer.

## Hur gör man:
```Ruby
require 'date'

# För att få ett datum 10 dagar framåt
future_date = Date.today + 10
puts future_date

# För att få ett datum 20 dagar bakåt
past_date = Date.today - 20
puts past_date
```
Exempelutskrift:
```
# Om dagens datum är 2023-04-01
2023-04-11
2023-03-12
```

## Djupdykning:
I Ruby är Datum-klassen den främsta mekanismen för att hantera datum. Det ingår i standardbiblioteket och lägger mycket flexibilitet till tidsberäkningar utan att behöva lita på externa bibliotek. Historiskt sätt har andra språk och bibliotek behandlat datum och tid på olika sätt, men många moderna språk, inklusive Ruby, erbjuder robusta inbyggda verktyg för sådana beräkningar.

Det finns alternativ till Datum-klassen i Ruby, såsom Time och ActiveSupport (del av Ruby on Rails ramverket) som också tillåter manipulation av datum och tid. Exempelvis kan ActiveSupport-medoden `advance` användas för mer avancerade datumoperationer – till exempel att räkna månader eller år framåt eller bakåt.

När man beräknar datum i förflutet eller framtiden bör man också tänka på skottår och tidszoner. Ruby hanterar skottår automatiskt men att arbeta med tidszoner kan ofta kräva ytterligare uppmärksamhet.

## Se även:
- Tid och datum i Ruby on Rails: [Ruby on Rails API](https://api.rubyonrails.org/classes/ActiveSupport/TimeWithZone.html)
- En översyn av tid och datum i olika programmeringsspråk: [“Your Calendrical Fallacy Is...”](https://yourcalendricalfallacyis.com)
