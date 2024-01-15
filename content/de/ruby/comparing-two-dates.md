---
title:                "Vergleich von zwei Daten"
html_title:           "Ruby: Vergleich von zwei Daten"
simple_title:         "Vergleich von zwei Daten"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/ruby/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Warum

Vergleichen von zwei Daten ist ein grundlegender Teil der Datentransformation und -analyse, sowohl in der Webentwicklung als auch in der allgemeinen Programmierung. Durch das Vergleichen von zwei Daten können Entwickler herausfinden, ob eine zeitliche Abhängigkeit besteht oder ob eine bestimmte Bedingung erfüllt ist.

## Wie man es macht

Der Vergleich von zwei Daten in Ruby ist relativ einfach. Zunächst müssen die beiden Daten in einem geeigneten Format gespeichert werden. Dann kann die `Date` Klasse von Ruby verwendet werden, um die Daten zu vergleichen.

```
require 'date'

date1 = Date.new(2020, 01, 01)
date2 = Date.new(2020, 01, 15)

if date1 == date2
  puts "Die beiden Daten sind gleich."
elsif date1 < date2
  puts "Die erste Datum ist vor dem zweiten Datum."
else
  puts "Die erste Datum ist nach dem zweiten Datum."
end
```

Die Ausgabe dieses Codes wird `Die erste Datum ist vor dem zweiten Datum.` sein.

## Tiefer eintauchen

Beim Vergleichen von Daten sollten Entwickler auch die Zeitzone berücksichtigen. In Ruby kann dies mit der `DateTime` Klasse durchgeführt werden. Ein weiterer wichtiger Aspekt ist, dass Daten unter Umständen in verschiedenen Formaten vorliegen können. In solchen Fällen kann die `parse` Methode verwendet werden, um Daten aus unterschiedlichen Formaten in ein einheitliches Format umzuwandeln.

[Dieser Artikel](https://www.rubyguides.com/2018/10/ruby-date-and-time/) bietet eine umfassende Übersicht über die Arbeit mit Daten und der `DateTime` Klasse in Ruby.

[Hier](https://www.rubyguides.com/2015/12/ruby-date-time/) findest du eine detailliertere Erklärung der `Date` Klasse und ihrer Funktionalitäten.

## Siehe auch

- [Dokumentation für die `Date` Klasse](https://ruby-doc.org/stdlib-2.4.1/libdoc/date/rdoc/Date.html)
- [Dokumentation für die `DateTime` Klasse](https://ruby-doc.org/stdlib-2.4.1/libdoc/date/rdoc/DateTime.html)
- [Einen Zeitstempel in Ruby erstellen](https://www.rubyguides.com/2019/01/ruby-timestamp/)