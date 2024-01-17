---
title:                "Jämföra två datum"
html_title:           "Ruby: Jämföra två datum"
simple_title:         "Jämföra två datum"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/comparing-two-dates.md"
---

{{< edit_this_page >}}

# Vad & Varför?

Att jämföra två datum är en vanlig uppgift för programmerare. Det är ett sätt att kontrollera om ett datum är tidigare, senare eller samma som ett annat datum. Detta kan vara användbart i många olika programmeringsprojekt, från kalenderapplikationer till finansiella system.

# Hur man gör:

```ruby
date1 = Date.new(2021, 3, 14)
date2 = Date.new(2021, 3, 21)

# Använd <=> operatorn för att jämföra två datum
puts date1 <=> date2
```

Output: -1

Detta betyder att date1 är tidigare än date2. Om resultatet var 0, skulle det betyda att de två datum är samma, och om det var 1, skulle date1 vara senare än date2.

# Djupdykning:

## Historisk kontext:
Jämförelse mellan datum har varit en utmaning för programmerare sedan tidiga dagar av datateknik. Innan standardfordonsgränssnittet för datum (ISO 8601) fastställdes var det vanligt med olika format och tolkningar av datum. Detta gjorde att jämförelse mellan datum ofta blev komplicerat och problematiskt.

## Alternativ:
Det finns flera olika sätt att jämföra datum i Ruby, inklusive användning av Date#eql? och Date#<=> metoder, eller genom att konvertera datum till integers och jämföra dem med == operatorn.

## Implementeringsdetaljer:
I Ruby är Date objekt baserade på det gregorianska kalendersystemet och har inbyggda metoder för att utföra olika operationer på datum, inklusive jämförelse.

# Se även:

- Ruby Date Klass Documentation: https://ruby-doc.org/stdlib-2.7.2/libdoc/date/rdoc/Date.html
- Jämföra datum i Ruby: https://www.rubyguides.com/2015/10/ruby-comparison-operators/ 
- Historia och utmaningar med datum i programmering: https://arstechnica.com/science/2009/12/the-perils-of-date-parsing-and-comparison/