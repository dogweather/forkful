---
title:                "Ruby: Att få den aktuella datumen"
programming_language: "Ruby"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Varför

Att hämta den nuvarande datumet är en vanlig uppgift som många programmerare kommer att stöta på i sitt arbete. Oavsett om du behöver visa den aktuella tiden på en webbapplikation eller använda den i beräkningar eller för att organisera data, är det viktigt att veta hur man enkelt kan hämta datumet med hjälp av Ruby-programmering.

## Hur man gör det

```Ruby
current_date = Time.now
puts current_date

#Återger: 2021-04-21 16:10:47 +0200
```

För att hämta det nuvarande datumet i Ruby, kan du använda den inbyggda metoden "Time.now". Detta returnerar ett objekt av klassen Time, som representerar den aktuella tiden. Med hjälp av "puts" kommandot kan du sedan skriva ut detta värde till konsolen.

Det finns också andra inbyggda metoder i Ruby som kan användas för att anpassa hur den nuvarande tiden visas, till exempel "strftime" för att ange det önskade formatet.

## Djupdykning

Att förstå hur klockan och datumet fungerar i en programmeringsspråk som Ruby är viktigt eftersom det kan påverka andra delar av din kod. Till exempel, om du behöver jämföra olika tidsvärden eller göra beräkningar baserat på datum, är det viktigt att lära dig hur man hanterar tidszoner och hur de påverkar den nuvarande tiden.

Det finns också många användbara juveler (gems) tillgängliga i Ruby för att hjälpa till med datumbehandling, till exempel "timecop" för att simulera olika tidpunkter och "chronic" för att översätta naturligt språk till datum och tider.

## Se även

- [Ruby Time dokumentation](https://ruby-doc.org/core-3.0.0/Time.html)
- [Stoverclock: Date and Time Handling in Ruby](https://www.sitepoint.com/ruby-date-time-handling/)
- [RubyGems.org](https://rubygems.org/)