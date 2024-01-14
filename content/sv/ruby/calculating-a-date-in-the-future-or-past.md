---
title:                "Ruby: Beräkna ett datum i framtiden eller det förflutna"
simple_title:         "Beräkna ett datum i framtiden eller det förflutna"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Varför

Att kunna beräkna datum i framtiden eller förflutna är en viktig färdighet för alla som arbetar med Ruby-programmering. Det kan hjälpa till att automatisera uppgifter och spara tid när man hanterar datum och tid i sina projekt.

## Hur man gör det

För att beräkna ett datum i framtiden eller förflutna behöver vi använda oss av Ruby's Date klass. Genom att använda metoden `+` eller `-` kan vi lägga till eller subtrahera ett visst antal dagar från ett datum. Till exempel:

```Ruby
Date.today + 7 #=> 2021-10-22
Date.today - 7 #=> 2021-10-08
```

Vi kan också använda metoden `advance` för att specificera hur många dagar, månader eller år vi vill lägga till eller subtrahera. Till exempel:

```Ruby
Date.today.advance(days: 14) #=> 2021-10-29
Date.today.advance(years: -2) #=> 2019-10-15
```

Om vi behöver beräkna ett specifikt datum, kan vi använda metoden `parse` tillsammans med parameter för ett datum i form av en sträng. Till exempel:

```Ruby
Date.parse('2021-10-15') + 10 #=> 2021-10-25
```

## Djupdykning

Det finns också fler avancerade sätt att beräkna datum i framtiden eller förflutna. En av dessa är att använda sig av `DateTime` klassen istället för `Date` klassen. Detta gör det möjligt för oss att även ta hänsyn till tiden när vi beräknar datum. Ett exempel:

```Ruby
DateTime.now + 2.hours #=> 2021-10-15 17:30:12 +0200
```

Det finns också en rad andra metoder som kan hjälpa oss att hantera datum och tid i Ruby, som `strftime`, `strptime` och `to_time`. Det är viktigt att läsa på dokumentationen för dessa metoder för en mer detaljerad förklaring.

## Se även

- [Officiell dokumentation för Ruby's Date och DateTime klasser](https://ruby-doc.org/stdlib-2.7.1/libdoc/date/rdoc/Date.html)
- [En guide till Ruby Date och Time klasser](https://www.rubyguides.com/2015/08/ruby-datetime/)
- [Beräkna datum i Ruby med Rails' ActiveSupport](https://www.rubyguides.com/2019/02/date-calculation-ruby/)