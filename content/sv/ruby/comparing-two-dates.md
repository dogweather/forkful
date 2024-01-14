---
title:                "Ruby: Jämföra två datum"
simple_title:         "Jämföra två datum"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Varför
Att jämföra två datum kan vara användbart när du bygger en app som involverar tidsbaserad funktionalitet, som till exempel en kalender eller bokningssystem. Att förstå hur man jämför datum kan hjälpa dig att hantera och manipulera tidsrelaterad data på ett effektivt sätt.

## Hur man jämför datum i Ruby
För att jämföra två datum i Ruby kan du använda operatorerna `<`, `>`, `<=` och `>=`. Dessa operatorer används för att jämföra värdet av två variabler och returnerar `true` om villkoret är sant och `false` om villkoret är falskt.

```Ruby
# Skapar två nya DateTime-objekt för jämförelse
date1 = DateTime.new(2021, 3, 10)
date2 = DateTime.new(2021, 3, 15)

# Jämför om date1 är mindre än date2
date1 < date2
=> true

# Jämför om date1 är större än date2
date1 > date2
=> false
```

För att jämföra datum med tid används `DateTime`-objekt och för att jämföra datum utan tid används `Date`-objekt. Du kan också använda metoden `compare` som ett alternativ till operatorerna `<` och `>`.

```Ruby
# Skapar två nya Date-objekt för jämförelse
date1 = Date.new(2021, 3, 10)
date2 = Date.new(2021, 3, 15)

# Jämför om date1 är mindre än eller lika med date2
date1 <= date2
=> true

# Jämför om date1 är större än eller lika med date2
date1 >= date2
=> false
```

## Djupdykning
När man jämför datum i Ruby kan man stöta på vissa utmaningar med förändringar i tidszoner och sommartid. För att hantera detta kan du använda metoden `local_compare` som tar hänsyn till tidszonen och sommartid. Du kan också använda metoden `to_time` för att omvandla DateTime-objekt till Time-objekt och enklare hantera tidsskillnader.

```Ruby
# Skapar DateTime-objekt med olika tidszoner
date1 = DateTime.new(2021, 3, 10, 12, 0, 0, '+01:00') # tidszon UTC +01:00
date2 = DateTime.new(2021, 3, 10, 12, 0, 0, '-03:00') # tidszon UTC -03:00

# Jämför om date1 och date2 är samma tidpunkt i olika tidszoner
date1.compare(date2)
=> 0 # returnerar 0 om de är samma tidpunkt i olika tidszoner

# Omvandla DateTime-objekt till Time-objekt
date1.to_time < date2.to_time
=> false # returnerar false eftersom nu finns det en tidskillnad på 4 timmar mellan dem
```

## Se även
- [Ruby DateTime documentation](https://ruby-doc.org/stdlib-2.7.2/libdoc/date/rdoc/DateTime.html)
- [Working with time zones in Ruby](https://www.rubyguides.com/2019/08/ruby-timezone/)
- [Comparing dates in Ruby](https://www.geeksforgeeks.org/comparison-of-date-in-ruby/)