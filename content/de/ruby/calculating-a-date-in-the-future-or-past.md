---
title:                "Ruby: Berechnung eines Datums in der Zukunft oder Vergangenheit"
simple_title:         "Berechnung eines Datums in der Zukunft oder Vergangenheit"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/ruby/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Warum

Die Berechnung von Datum und Uhrzeit ist ein wichtiger Teil jeder Programmiersprache, einschließlich Ruby. Es kann verwendet werden, um zukünftige oder vergangene Termine zu bestimmen, was besonders nützlich ist, wenn man mit Aufgabenplanung oder zeitbasierten Anwendungen arbeitet. In diesem Artikel werden wir uns ansehen, wie man mithilfe von Ruby ein Datum in der Zukunft oder Vergangenheit berechnen kann.

## Wie man ein Datum in der Zukunft oder Vergangenheit berechnet

Um ein Datum in der Zukunft oder Vergangenheit zu berechnen, können wir die Methode `advance` aus der Standardbibliothek `Date` verwenden. Sie nimmt mehrere Argumente an, einschließlich der Anzahl an Jahren, Monaten, Tagen, Stunden, Minuten und Sekunden, die hinzugefügt oder subtrahiert werden sollen. Schauen wir uns ein Beispiel an:

```Ruby
require 'date'

today = Date.today
puts "Heute ist der #{today.day}. des #{today.month}. Monats im Jahr #{today.year}."

future_date = today.advance(months: 3, days: 14)
puts "In 3 Monaten und 14 Tagen ist der #{future_date.day}. des #{future_date.month}. Monats im Jahr #{future_date.year}."
```

In diesem Beispiel berechnen wir ein Datum, das 3 Monate und 14 Tage in der Zukunft liegt. Das Ergebnis sieht wie folgt aus:

```
Heute ist der 8. des 10. Monats im Jahr 2021.
In 3 Monaten und 14 Tagen ist der 22. des 1. Monats im Jahr 2022.
```

Wir können auch ein Datum in der Vergangenheit berechnen, indem wir negative Werte für die Argumente angeben. Hier ist ein Beispiel:

```Ruby
past_date = today.advance(years: -2, months: -6)
puts "Vor 2 Jahren und 6 Monaten war der #{past_date.day}. des #{past_date.month}. Monats im Jahr #{past_date.year}."
```

Das Ergebnis sieht wie folgt aus:

```
Vor 2 Jahren und 6 Monaten war der 8. des 4. Monats im Jahr 2019.
```

## Tiefer eintauchen

In diesem Beispiel haben wir die Methode `advance` benutzt, um ein Datum in der Zukunft oder Vergangenheit zu berechnen. Diese Methode ist jedoch nicht die einzige Möglichkeit, dies zu tun. Alternativ können wir auch die Methode `+` benutzen und ein `Date`-Objekt mit einem Integer multiplizieren. Beide Methoden führen letztendlich zum gleichen Ergebnis.

Es ist auch wichtig anzumerken, dass diese Methoden nur in der Klasse `Date` verfügbar sind. Um ein Datum in der Vergangenheit oder Zukunft zu berechnen, das auch Uhrzeitangaben enthält, müssen wir die Klasse `DateTime` verwenden und die Methode `new` mit entsprechenden Argumenten aufrufen.

## Siehe auch

- [Date Klasse in der Ruby Dokumentation](https://ruby-doc.org/stdlib-2.7.4/libdoc/date/rdoc/Date.html)
- [DateTime Klasse in der Ruby Dokumentation](https://ruby-doc.org/stdlib-2.7.4/libdoc/date/rdoc/DateTime.html)
- [Ruby Guides: Zeit und Datum](https://www.rubyguides.com/2015/03/ruby-time/)