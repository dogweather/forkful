---
date: 2024-01-20 17:33:38.307515-07:00
description: "Hvordan: Ruby har hatt innebygd st\xF8tte for datoer siden de tidlige\
  \ dager med `Date` klassen. Klassen gir metoder for \xE5 sammenligne datoer (for\
  \ eksempel\u2026"
lastmod: '2024-04-05T21:53:42.288905-06:00'
model: gpt-4-1106-preview
summary: "Ruby har hatt innebygd st\xF8tte for datoer siden de tidlige dager med `Date`\
  \ klassen."
title: Sammenlikning av to datoer
weight: 27
---

## Hvordan:
```Ruby
require 'date'

# Opprett to Date objekter
date1 = Date.new(2023, 4, 10)
date2 = Date.new(2023, 4, 12)

# Sammenligne datoer
puts date1 < date2  # Output: true
puts date1 > date2  # Output: false
puts date1 == date2 # Output: false
puts date1.eql?(date2) # Output: false
puts date1.equal?(date2) # Output: false

# Differanse mellom datoer i dager
puts (date2 - date1).to_i # Output: 2
```

## Dypdykk
Ruby har hatt innebygd støtte for datoer siden de tidlige dager med `Date` klassen. Klassen gir metoder for å sammenligne datoer (for eksempel `<`, `>`, `==`, `eql?` og `equal?`) og for å regne ut differansen mellom dem. Differansen returneres som et `Rational` nummer som kan omformes til et heltall med `to_i` for å få antall dager.

Som et alternativ til `Date` klassen, kan man også bruke `Time` for mer nøyaktige tidspunkt, inkludert klokkeslett. I et distribuert system kan man trenge å ta hensyn til tidszoner, og da er `ActiveSupport::TimeWithZone` fra Rails en god løsning.

Når det kommer til implementasjon, vurderer Ruby objekt-idene når `equal?` brukes (to objekter er nøyaktig det samme objektet), mens `eql?` og `==` sammenligner verdiene av de to datoene for å finne ut om de representerer samme dag.

## Se Også:
- [Ruby Date Class Documentation](https://ruby-doc.org/stdlib-3.1.2/libdoc/date/rdoc/Date.html)
- [Ruby Time Class Documentation](https://ruby-doc.org/core-3.1.2/Time.html)
- [Rails ActiveSupport TimeWithZone](https://api.rubyonrails.org/classes/ActiveSupport/TimeWithZone.html)
