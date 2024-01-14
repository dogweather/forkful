---
title:    "Ruby: Jämföra två datum"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Varför
Att jämföra två datum är en vanlig uppgift inom programmering, speciellt inom Ruby. Genom att jämföra datum kan vi avgöra om ett visst datum kommer före eller efter ett annat, vilket kan vara användbart i olika applikationer. I denna bloggpost kommer vi att undersöka hur man jämför datum i Ruby och vad som händer när man gör det.

## Hur
För att jämföra två datum i Ruby, kan vi använda metoden `Date#<=>` som står för "less than, equal, greater than". Den här metoden returnerar -1, 0 eller 1 beroende på om det första datumet är mindre än, lika med eller större än det andra datumet. 

```Ruby
require 'date'

date1 = Date.parse('2021-01-01')
date2 = Date.parse('2020-12-31')

puts date1 <=> date2

# Output: 1 (eftersom date1 kommer efter date2)
```

Om de två jämförda datumen är exakt samma, returnerar metoden 0.

```Ruby
require 'date'

date1 = Date.parse('2021-01-01')
date2 = Date.parse('2021-01-01')

puts date1 <=> date2

# Output: 0
```

Om vi istället vill kontrollera om ett datum är tidigare eller senare än ett annat kan vi använda metoden `Date#<` för mindre än och `Date#>` för större än.

```Ruby
require 'date'

date1 = Date.parse('2021-01-01')
date2 = Date.parse('2020-12-31')

puts date1 < date2
# Output: false

puts date1 > date2
# Output: true
```

## Deep Dive
När vi jämför datum i Ruby, jämförs de baserat på en standard som kallas "Julian day". Det är en numreringssystem som börjar den 24 november år 4714 f.Kr. Detta är det datum då den första julianska kalendern började räkna dagar. Varje dag har ett unikt nummer och det är detta nummer som används för att jämföra datumen.

Det finns också några saker som är viktiga att tänka på när vi jämför datum i Ruby. För det första, måste de vara av samma typ, antingen `Date`, `DateTime` eller `Time`. Om vi försöker jämföra datumen i olika format (t.ex. `Date` och `DateTime`) kan vi få ett felmeddelande.

För det andra, när vi jämför datumen så ingår även tid i jämförelsen. Detta betyder att ett datum med tiden 00:00 kommer att anses vara mindre än ett datum med tiden 12:00, även om de är på samma dag.

## Se även
* [Ruby DateTime documentation](https://ruby-doc.org/stdlib-2.6.1/libdoc/date/rdoc/DateTime.html)
* [How to compare two dates in Ruby](https://www.rubyguides.com/2019/03/date-comparison/)
* [Ruby Date and Time class](https://www.techotopia.com/index.php/Ruby_Date_and_Time_Class_Members)