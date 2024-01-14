---
title:                "Ruby: Vergleich von zwei Daten"
simple_title:         "Vergleich von zwei Daten"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/ruby/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Warum

Warum sollte man sich mit dem Vergleichen von zwei Daten beschäftigen? Nun, viele Programme, die man schreibt, benötigen Logik, um mit Daten umzugehen, die sich im Laufe der Zeit verändern. Das Vergleichen von Datumsangaben ist daher ein wichtiger Teil des Programmierens.

## Wie man Datumsangaben vergleicht

```Ruby
# Beispielcode zum Vergleichen von Datumsangaben
date_1 = Date.parse("2020-01-01")
date_2 = Date.today

if date_1 > date_2
  puts "Das erste Datum liegt in der Zukunft."
elsif date_1 < date_2
  puts "Das erste Datum liegt in der Vergangenheit."
else
  puts "Beide Daten sind identisch."
end
```

In diesem Beispiel zeigen wir, wie man mit Hilfe von Vergleichsoperatoren wie ">" (größer als) und "<" (kleiner als) zwei Datumsangaben vergleicht. Die Ausgabe hängt davon ab, welches Datum in der Zukunft bzw. Vergangenheit liegt.

## Tiefere Einblicke

Das Vergleichen von Datumsangaben kann etwas komplizierter werden, sobald wir auch die Uhrzeit berücksichtigen. Auch müssen wir möglicherweise bedenken, dass unterschiedliche Zeitzonen eine Rolle spielen können. Es gibt also einige Faktoren, die bei der Programmierung und dem Vergleichen von Datumsangaben beachtet werden sollten.

## Siehe auch

- [Ruby Date Klasse Dokumentation](https://ruby-doc.org/stdlib-2.7.2/libdoc/date/rdoc/Date.html)
- [Vergleichsoperatoren in Ruby](https://www.rubyguides.com/2017/01/ruby-comparison-operators/)
- [Ruby Zeitzonen mit tzinfo](https://tzinfo.github.io/)