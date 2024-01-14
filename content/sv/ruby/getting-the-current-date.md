---
title:    "Ruby: Hämta aktuellt datum"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Varför

Att kunna hämta den aktuella datumet är en viktig del av programmering. Det kan användas för att spåra tidsstämplar för data eller för att visa aktuellt datum på en webbplats. Låt oss fördjupa oss i hur man kan göra detta med hjälp av Ruby-programmering.

## Hur man gör

För att hämta det aktuella datumet i Ruby, behöver vi använda oss av klassen "Date" och dess metod "today".

```Ruby
require 'date'

puts Date.today
```
Output: 2021-10-11

Om vi vill ändra formatet på datumet kan vi använda metoderna "strftime" och "strptime".

```Ruby
require 'date'

puts Date.today.strftime("%d/%m/%Y")
```
Output: 11/10/2021

Vi kan också ange ett specifikt datum och få det att returneras i Date-format med hjälp av "strptime" metoden.

```Ruby
require 'date'

puts Date.strptime("01/01/2021", "%d/%m/%Y")
```
Output: 2021-01-01

## Fördjupning

Som nämnts tidigare är "Date" en klass i Ruby som ger oss tillgång till metoden "today" för att hämta det aktuella datumet. Denna klass har också andra användbara metoder som låter oss jämföra datum och utföra olika operationer på dem.

En annan viktig aspekt att veta är att datumet som returneras är i UTC (koordinerad universell tid) format. Om du behöver konvertera det till en annan tidszon kan du använda "localtime" metod.

```Ruby
require 'date'

puts Date.today.localtime("+02:00")
```
Output: 2021-10-11

## Se också

- Ruby Dokumentation om Date-klassen: https://ruby-doc.org/stdlib-3.0.2/libdoc/date/rdoc/Date.html
- En tutorial om hur man hanterar datum och tid i Ruby: https://www.rubyguides.com/2015/09/working-with-dates-and-time-in-ruby/
- En diskussion om användbarheten av "Date" klassen i Ruby: https://stackoverflow.com/questions/7963581/how-to-manipulate-dates-in-ruby