---
title:                "Ruby: Vergleich von zwei Daten"
programming_language: "Ruby"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/ruby/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Warum

Das Vergleichen von zwei Daten kann hilfreich sein, um zum Beispiel zu überprüfen, ob ein bestimmtes Ereignis in der Zukunft liegt oder bereits in der Vergangenheit stattgefunden hat. Dies kann nützlich sein für die Programmierung von Kalendern, Zeitplanern oder anderen Anwendungen, die mit Zeiten und Terminen arbeiten.

## Wie

Um zwei Daten in Ruby zu vergleichen, können wir die Ruby-Methode `Date#compare` verwenden. Diese Methode vergleicht zwei Daten und gibt entweder -1, 0 oder 1 zurück, je nachdem, ob das erste Datum vor, gleich oder nach dem zweiten Datum liegt.

```Ruby
first_date = Date.new(2020, 5, 1)
second_date = Date.new(2020, 5, 5)

puts first_date.compare(second_date)
```
Dieser Code würde 1 ausgeben, da das erste Datum (2020-05-01) nach dem zweiten Datum (2020-05-05) liegt.

Wir können auch die `Date#<=>` Methode verwenden, die ähnlich funktioniert, aber anstelle von -1, 0 oder 1, einen Wert zwischen -1 und 1 zurückgibt.

```Ruby
puts first_date <=> second_date
```
Dieser Code würde -1 ausgeben, da das erste Datum vor dem zweiten Datum liegt.

## Tiefergehende Informationen

Es gibt viele Faktoren, die bei der Verwendung von Datumsklassen in Ruby zu beachten sind, wie zum Beispiel die Berücksichtigung von Zeitverschiebungen oder unterschiedlichen Zeitzonen. Es ist wichtig, sich über diese Faktoren bewusst zu sein und möglicherweise zusätzliche Methoden zu verwenden, wie zum Beispiel `Time#compare`, um eine genauere Vergleichsfunktion zu erhalten.

## Siehe auch

- [Ruby Date Klasse Dokumentation](https://ruby-doc.org/stdlib-2.7.0/libdoc/date/rdoc/Date.html)
- [Warum Ruby Date Objekte besser als Zeit sind](https://thoughtbot.com/blog/why-ruby-date-objects-are-better-than-time)
- [Vergleichen von Zeitstempeln mit Ruby](https://stackoverflow.com/questions/108820/foo-to-compare-dates-in-ruby-on-rails)