---
title:                "Ruby: Berechnung eines Datums in der Zukunft oder Vergangenheit"
programming_language: "Ruby"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/ruby/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Warum

Es gibt viele Gründe, warum jemand ein Datum in der Zukunft oder Vergangenheit berechnen möchte. Vielleicht planst du eine Reise und möchtest wissen, an welchem Datum du zurück sein wirst. Oder du musst ein Projekt mit einem bestimmten Fertigstellungsdatum planen. Mit Ruby kannst du ganz einfach Datumsberechnungen durchführen, die dir dabei helfen können, diese Aufgaben zu bewältigen.

## Wie man vorgeht

In Ruby gibt es eine eingebaute Methode namens `DateTime`, die uns bei der Berechnung von Datumsangaben unterstützt. Zunächst müssen wir jedoch die Bibliothek `date` importieren, um auf diese Methode zugreifen zu können.

**Beispiel 1:** Um das Datum von heute auszugeben, können wir folgenden Code verwenden:

```Ruby
require 'date'
puts Date.today
```

Dieser Code importiert die `date` Bibliothek und gibt dann die aktuelle Datum und Uhrzeit aus. Das Ergebnis sieht etwa so aus: `#<Date: 2021-10-15 ((2459491j,0s,0n),+0s,2299161j)>`

**Beispiel 2:** Wenn wir ein Datum in der Zukunft berechnen möchten, können wir das `DateTime` Objekt verwenden. Schau dir folgendes Beispiel an:

```Ruby
require 'date'
future_date = DateTime.now + 7
puts "Das Datum in einer Woche ist: #{future_date}"
```

Dieser Code importiert die `date` Bibliothek und berechnet dann das Datum von heute aus betrachtet plus 7 Tage. Das Ergebnis sieht etwa so aus: `Das Datum in einer Woche ist: 2021-10-22T00:00:00+00:00`

**ACHTUNG:** Stelle sicher, dass du bei der Datumsberechnung auch Schaltjahre berücksichtigst, um genaue Ergebnisse zu erhalten.

## Tiefere Einblicke

Es gibt viele weitere Methoden und Techniken, die du bei der Berechnung von Datum in Ruby verwenden kannst. Du kannst z. B. auch bestimmte Wochentage oder Zeitzonen berücksichtigen. Wenn du mehr über das Thema lernen möchtest, empfehle ich dir, folgende Links zu besuchen:

- [Offizielle Dokumentation zu `DateTime` in Ruby](https://ruby-doc.org/core-3.0.1/DateTime.html)
- [Ein einfacher Guide zur Datumsberechnung in Ruby](https://www.rubyguides.com/2015/10/ruby-date-time-tutorial/)
- [Ruby Datums- und Zeit-Manipulation Tipps und Tricks](https://www.codementor.io/@garethdwyer/ruby-date-and-time-manipulation-tips-and-tricks-part-2-4dmi6cvmu)

## Siehe auch

- [Offizielle Ruby Dokumentation](https://www.ruby-lang.org/de/documentation/)
- [Ruby Programmierung für Anfänger](https://www.ruby-lang.org/de/documentation/ruby-from-other-languages/to-ruby-from-python/)