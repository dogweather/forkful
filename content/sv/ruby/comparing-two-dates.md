---
title:                "Jämför två datum"
html_title:           "Arduino: Jämför två datum"
simple_title:         "Jämför två datum"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att jämföra två datum innebär att fastställa vilket datum som kommer före eller efter det andra. Programmerare gör detta för att ordna datumsekvenser och för att utföra tidsberoende beräkningar.

## Så här gör du:
Ruby har inbyggda klasser för att hantering av datum. Här är ett enkelt exempel på hur man jämför två datum:

```Ruby
require 'date'

date_one = Date.new(2021,3,15)
date_two = Date.new(2023,2,28)

if date_one < date_two
  puts "date_one kommer före date_two"
else
  puts "date_one kommer efter date_two"
end
```
Exempelutdata:

```Ruby
"date_one kommer före date_two"
```

## Fördjupning
Ruby har alltid prioriterat läsbarhet och enkelhet, vilket är anledningen till att jämförelse av datum är mycket enkelt. Men innan datumklassen introducerades, var jämförelse av datum i Ruby mer komplicerat och involverade att hantera individuella år, månad och dag komponenter.

Ett alternativ till att använda datumklassen är att använda Time-klassen, men den är oftast mer lämplig för att jämföra specifika tidpunkter snarare än hela datum. En annan viktig detalj med implementeringen är att operatörer, såsom '<' och '>', är överbelastade i Date-klassen till att jämföra datum.

## Se också
För mer information om datumhantering i Ruby, se följande resurser:

1. Ruby Datum och Tid Dokumentation: https://ruby-doc.org/stdlib-2.5.1/libdoc/date/rdoc/Date.html
2. Ruby Time Dokumentation: https://ruby-doc.org/core-2.5.0/Time.html
3. Jämförelse metoder i Ruby: https://ruby-doc.org/core-2.6.1/Comparable.html