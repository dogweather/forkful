---
date: 2024-01-20 17:33:50.793272-07:00
description: "How to: (Comment faire:) Comparer des dates n'est pas nouveau. Depuis\
  \ les ann\xE9es 60, les syst\xE8mes informatiques calculent l'\xE9cart entre des\
  \ dates pour des\u2026"
lastmod: '2024-04-05T22:51:12.287955-06:00'
model: gpt-4-1106-preview
summary: (Comment faire:) Comparer des dates n'est pas nouveau.
title: Comparer deux dates
weight: 27
---

## How to: (Comment faire:)
```Ruby
require 'date'

date_1 = Date.new(2023, 4, 15)
date_2 = Date.new(2023, 10, 31)

puts date_1 < date_2   # => true
puts date_1 == date_2  # => false
puts date_1 > date_2   # => false
puts date_1.upto(date_2).to_a # => [date_1, date_1+1,..., date_2]
```

## Deep Dive (Plongée Profonde)
Comparer des dates n'est pas nouveau. Depuis les années 60, les systèmes informatiques calculent l'écart entre des dates pour des opérations bancaires ou des réservations aériennes. Ruby offre une classe `Date` simple mais puissante. Alternativement, `Time` et `DateTime` offrent plus de précision. L'implémentation interne gère les anomalies comme les années bissextiles et le passage à l'heure d'été.

## See Also (Voir Aussi)
- La documentation Ruby sur les classes `Date`, `Time` et `DateTime`: https://ruby-doc.org/standard-library/libdoc/date/rdoc/Date.html
- Un guide sur la gestion du temps en Ruby: https://www.rubyguides.com/2015/12/ruby-time/
- Comparaison approfondie entre `Date`, `Time`, et `DateTime`: https://ruby-doc.org/stdlib-2.4.0/libdoc/date/rdoc/DateTime.html
