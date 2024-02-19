---
aliases:
- /fr/ruby/comparing-two-dates/
date: 2024-01-20 17:33:50.793272-07:00
description: "Comparer deux dates en Ruby permet de d\xE9terminer laquelle est ant\xE9\
  rieure ou post\xE9rieure, ou bien si elles sont identiques. C'est crucial pour des\u2026"
lastmod: 2024-02-18 23:09:09.420895
model: gpt-4-1106-preview
summary: "Comparer deux dates en Ruby permet de d\xE9terminer laquelle est ant\xE9\
  rieure ou post\xE9rieure, ou bien si elles sont identiques. C'est crucial pour des\u2026"
title: Comparer deux dates
---

{{< edit_this_page >}}

## What & Why? (Quoi et Pourquoi ?)
Comparer deux dates en Ruby permet de déterminer laquelle est antérieure ou postérieure, ou bien si elles sont identiques. C'est crucial pour des fonctionnalités comme la validation de la période de garantie, l'organisation d'événements, ou la gestion de réservations.

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
