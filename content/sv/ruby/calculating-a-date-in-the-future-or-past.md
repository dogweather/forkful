---
title:                "Beräkna ett datum i framtiden eller i det förflutna"
html_title:           "Ruby: Beräkna ett datum i framtiden eller i det förflutna"
simple_title:         "Beräkna ett datum i framtiden eller i det förflutna"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att beräkna ett datum i framtiden eller det förflutna är en vanlig uppgift inom programmering. Vanligtvis görs detta för att program behöver planera eller agera baserat på specifika tidsramar och datum. Till exempel kan en kalenderapplikation behöva beräkna datumet för en uppgift i framtiden, eller en bokningssida kan behöva beräkna datumet för en återbetalning baserat på en avbokning.

## Hur man gör:

Att beräkna ett datum i framtiden eller det förflutna i Ruby är enkelt med hjälp av innebyggda funktioner och metoder. För att beräkna en specifik dag i framtiden kan vi använda `Date` klassens `+` metoden tillsammans med antalet dagar vi vill lägga till. Exempelvis, för att beräkna datumet för två veckor framåt:

```Ruby
require 'date'
puts Date.today + 14
```

Output: 2021-08-30

På samma sätt kan vi använda `-` metoden för att beräkna datumet för ett visst antal dagar tillbaka i tiden. Exempelvis, för att beräkna datumet för tre dagar sedan: 

```Ruby
require 'date'
puts Date.today - 3
```

Output: 2021-08-13

För att beräkna ett specifikt datum i det förflutna eller framtiden kan vi använda `Date.new` metoden tillsammans med det önskade året, månaden och dagen. Exempelvis, för att beräkna datumet för min födelsedag år 1990:

```Ruby
puts Date.new(1990, 9, 10)
```

Output: 1990-09-10

## Djupdykning:

Det finns flera olika sätt att beräkna ett datum i framtiden eller det förflutna i Ruby, såsom att använda `DateTime` klassen istället för `Date` för att även inkludera tid eller att använda andra metoder som `next_year` eller `prev_year` för att beräkna ett datum baserat på år istället för dagar. Det finns även externa bibliotek såsom `Chronic` som gör det möjligt att beräkna datum baserat på naturligt språk.

## Se även:

- [Ruby dokumentation för Date klassen](https://ruby-doc.org/stdlib-2.7.1/libdoc/date/rdoc/Date.html)
- [Chronic biblioteket för att hantera naturligt språk i datum beräkningar](https://github.com/mojombo/chronic)