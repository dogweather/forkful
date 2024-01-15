---
title:                "Omvandla ett datum till en sträng"
html_title:           "Ruby: Omvandla ett datum till en sträng"
simple_title:         "Omvandla ett datum till en sträng"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Varför

Att kunna konvertera ett datum till en sträng kan vara användbart i många sammanhang, till exempel när man vill visa ett datum på ett specifikt format eller när man vill jämföra olika datum. Ruby har inbyggda metoder för att enkelt utföra denna konvertering, vilket gör det till en viktig del av språket att behärska.

## Hur man gör det

För att konvertera ett datum till en sträng i Ruby använder man metoden `strftime`. Detta är en förkortning för "string from time", vilket indikerar att den är speciellt utformad för denna uppgift. Den tar emot ett argument som anger det format som strängen ska ha, och returnerar en sträng baserat på datumet som anges.

Till exempel, om man vill konvertera dagens datum till en sträng som visar månad, dag och år i Amerikanskt format, kan man använda följande kod:

```ruby
date = Date.today
puts date.strftime("%m/%d/%Y")
```

Detta kommer att returnera strängen "10/29/2020". Det finns många olika format man kan använda sig av, och det kan vara en bra idé att experimentera med olika format för att hitta det som passar bäst för ens specifika behov.

## Djupdykning

Det finns många olika format som kan användas med `strftime`, men några av de vanligaste och mest användbara är:

- `%m` för månad (i numerisk form)
- `%d` för dag i månaden
- `%Y` för år i fyra siffror
- `%b` för trebokstavskod för månad (ex. Jan, Feb, Mar)
- `%B` för helnamn på månad (ex. January, February, March)
- `%A` för helnamn på veckodag (ex. Monday, Tuesday, Wednesday)

Det finns många fler format än så, och de kan även kombineras för att skapa mer specifika strängar. Det är också möjligt att ange en landkod som argument till metoden, vilket gör det möjligt att få datumet och tiden i olika tidszoner.

## Se även

För mer information om `strftime` och andra metoder för att hantera datum och tider i Ruby, besök följande länkar:

- [Ruby's official documentation on strftime](https://ruby-doc.org/core-2.7.2/Time.html#method-i-strftime)
- [Date and Time formatting in Ruby](https://www.dummies.com/web-design-development/ruby/formatting-date-times-ruby/)
- [Mastering Date and Time in Ruby](https://thoughtbot.com/blog/mastering-dates-and-times-in-ruby)