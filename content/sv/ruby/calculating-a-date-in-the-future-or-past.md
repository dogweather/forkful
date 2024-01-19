---
title:                "Beräkna ett datum i framtiden eller förflutna"
html_title:           "Ruby: Beräkna ett datum i framtiden eller förflutna"
simple_title:         "Beräkna ett datum i framtiden eller förflutna"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Beräkna Datum i Ruby

## Varför och Varför?

Beräkning av ett datum i framtiden eller förflutet innebär att bestämma ett specifikt datum baserat på ett annat datum och en tidsperiod. Programmerare gör detta för att hantera uppgifter som schemaläggning, uppföljning och beräkning av tidsfrister.

## Hur man gör det:

I Ruby kan du beräkna ett datum i framtiden eller förflutet med `Date`-klassen. Använd metoden `+` för att räkna fram ett datum och metoden `-` för att räkna tillbaka.

```ruby
require 'date'

today = Date.today
puts "Idag är det: #{today}"

future_date = today + 30
puts "Om 30 dagar blir det: #{future_date}"

past_date = today - 30
puts "För 30 dagar sedan var det: #{past_date}"
```

## Djupdykning

Ruby's `Date`-klass introducerades i Ruby 1.9 för att ge inbyggda verktyg för datumhantering. Det finns alternativ som Time och DateTime, men Date används ofta för dess enkelhet och effektivitet när det handlar om rena datum, utan klockslag.

En detalj att notera är att `Date`-klassen i Ruby följer Gregorianska kalendern för alla datum, så du behöver inte oroa dig för skottårsberäkningar.

## Se även

Om du vill veta mer om hur du hanterar datum och tid i Ruby kan följande källor vara till hjälp:

- [Ruby Dokumentation för Date Klass](https://ruby-doc.org/stdlib-2.6.1/libdoc/date/rdoc/Date.html)
- [Ruby Guides: Working with Dates in Ruby](https://www.rubyguides.com/2015/12/ruby-time/)
- [Date and Time manipulation in Ruby](https://www.tutorialspoint.com/ruby/ruby_date_time.htm)