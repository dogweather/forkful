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

# Was & Warum?

Vergleichen Sie zwei Termine ist eine häufige Aufgabe in der Programmierung. Dabei werden zwei Datumswerte miteinander verglichen, um festzustellen, welches davon größer oder kleiner ist. Programmierer verwenden dies oft, um Bedingungen in ihren Programmen zu überprüfen oder um Daten zu sortieren.

# Wie geht das?

Vergleichen von zwei Terminen in Ruby ist ziemlich einfach. Nehmen wir an, wir haben zwei Termine als String-Werte:
```Ruby
date_one = "2021-05-18"
date_two = "2020-03-15"
```
Um diese beiden Termine miteinander zu vergleichen, können wir den eingebauten Vergleichsoperator ```<=>``` verwenden:
```Ruby
date_one <=> date_two
```
Dies gibt uns entweder 1, 0 oder -1 zurück, je nachdem welches Datum größer ist. Wenn ```date_one``` größer ist als ```date_two```, erhalten wir eine 1, wenn sie gleich sind eine 0 und wenn ```date_one``` kleiner ist als ```date_two```, erhalten wir eine -1.

# Tiefere Einblicke

Das Vergleichen von zwei Terminen stammt aus der Programmiersprache ALGOL 68 und wurde seitdem von vielen Sprachen übernommen, einschließlich Ruby. Alternativ können Programmierer auch die ```Date```-Klasse in Ruby verwenden, um eine breitere Palette von Vergleichsoperationen durchzuführen.

Der Vergleichsoperator ```<=>``` vergleicht nicht nur zwei Zeichenfolgen, sondern auch Objekte, die das Modul ```Comparable``` implementieren. Dies ermöglicht es Programmierern, benutzerdefinierte Klassen und Objekte miteinander zu vergleichen.

# Siehe auch

- [Ruby-Dokumentation: Vergleichsoperatoren](https://ruby-doc.org/core-3.0.0/doc/syntax/precedence_rdoc.html#label-Comparison+Operators)
- [Ruby-Dokumentation: Datumsklasse](https://ruby-doc.org/stdlib-3.0.0/libdoc/date/rdoc/Date.html)
- [Geschichte von Vergleichsoperatoren](https://en.wikipedia.org/wiki/Comparison_of_programming_languages_(syntax)#Comparison)