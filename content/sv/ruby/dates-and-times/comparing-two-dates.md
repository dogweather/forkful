---
aliases:
- /sv/ruby/comparing-two-dates/
date: 2024-01-20 17:33:47.457858-07:00
description: "Att j\xE4mf\xF6ra tv\xE5 datum inneb\xE4r att avg\xF6ra vilket datum\
  \ som kommer f\xF6re eller efter det andra, eller om de \xE4r samma. Programmerare\
  \ g\xF6r detta f\xF6r att hantera\u2026"
lastmod: 2024-02-18 23:08:52.307027
model: gpt-4-1106-preview
summary: "Att j\xE4mf\xF6ra tv\xE5 datum inneb\xE4r att avg\xF6ra vilket datum som\
  \ kommer f\xF6re eller efter det andra, eller om de \xE4r samma. Programmerare g\xF6\
  r detta f\xF6r att hantera\u2026"
title: "J\xE4mf\xF6ra tv\xE5 datum"
---

{{< edit_this_page >}}

## Vad & Varför?
Att jämföra två datum innebär att avgöra vilket datum som kommer före eller efter det andra, eller om de är samma. Programmerare gör detta för att hantera bokningar, deadlines, tidslinjer och andra tidsberoende funktioner.

## Hur gör man?
```ruby
require 'date'

datum1 = Date.new(2023, 3, 15)
datum2 = Date.new(2023, 4, 20)

puts datum1 < datum2  # Output: true
puts datum1 == datum2 # Output: false
puts datum1 > datum2  # Output: false
puts (datum2 - datum1).to_i # Antal dagar mellan datum: Output: 36
```

## Djupdykning
I Ruby hanteras datum med `Date`-klassen, som har funnits sedan den första versionen. Svårigheterna med datumjämförelser inkluderar tidszoner och skottår, något som Ruby hanterar automatiskt. Alternativ till `Date` inkluderar `Time` och `DateTime`, som även de erbjuder rik funktionalitet för att jämföra datum och tider. Specifika implementeringsdetaljer hänvisar till att `Date`-objekt i Ruby jämförs med stöd av operatoröverlagring, vilket innebär att operands, som `<`, `==`, och `>`, kan användas i en naturlig syntax som om de vore vanliga tal.

## Se även
- Ruby's officiella dokumentation för `Date` klassen: [Ruby Date Documentation](https://ruby-doc.org/stdlib-3.1.1/libdoc/date/rdoc/Date.html)
- Tutorial för hur man hanterar tidszoner i Ruby: [Working with Time Zones in Ruby](https://thoughtbot.com/blog/its-about-time-zones)
