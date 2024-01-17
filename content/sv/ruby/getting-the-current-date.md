---
title:                "Hämta aktuellt datum"
html_title:           "Ruby: Hämta aktuellt datum"
simple_title:         "Hämta aktuellt datum"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att hämta aktuellt datum är en vanlig uppgift inom programmering, eftersom det är användbart för att spåra tid och datum för olika händelser eller för att utföra beräkningar som är beroende av tiden. Det är också användbart för att skapa dynamiskt innehåll på webbplatser eller appar baserat på det aktuella datumet.

## Hur:
För att hämta det aktuella datumet i Ruby kan du använda den inbyggda metoden `Date.today`. Detta returnerar ett datumobjekt med dagens datum.

```Ruby
require 'date'

puts Date.today
#Output: 2021-09-01
```

Du kan också använda `Time`-klassen för att få mer detaljerad information om det aktuella datumet och tiden.

```Ruby
puts Time.now
#Output: 2021-09-01 15:30:00 +0200
```

## Djupdykning:
Att kunna hämta det aktuella datumet är en viktig del av dataprogrammering, särskilt inom områden som finans, logistik och planering. Det finns också andra tillvägagångssätt för att hantera datum, som att använda externa bibliotek eller att manipulera datumobjekt med hjälp av metoder som `strftime` för att formatera datumet på ett specifikt sätt.

## Se även:
- [Ruby Date-klassen dokumentation](https://ruby-doc.org/stdlib-2.7.2/libdoc/date/rdoc/Date.html)
- [Ruby Time-klassen dokumentation](https://ruby-doc.org/core-2.7.2/Time.html)
- [DateTime biblioteket](https://ruby-doc.org/stdlib-2.7.2/libdoc/datetime/rdoc/DateTime.html)