---
date: 2024-01-20 17:33:47.457858-07:00
description: "Hur g\xF6r man? I Ruby hanteras datum med `Date`-klassen, som har funnits\
  \ sedan den f\xF6rsta versionen. Sv\xE5righeterna med datumj\xE4mf\xF6relser inkluderar\
  \ tidszoner\u2026"
lastmod: '2024-04-05T21:53:39.782176-06:00'
model: gpt-4-1106-preview
summary: "I Ruby hanteras datum med `Date`-klassen, som har funnits sedan den f\xF6\
  rsta versionen."
title: "J\xE4mf\xF6ra tv\xE5 datum"
weight: 27
---

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
