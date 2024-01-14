---
title:                "Ruby: Die aktuelle Datum erhalten"
programming_language: "Ruby"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/ruby/getting-the-current-date.md"
---

{{< edit_this_page >}}

##Warum

Das aktuelle Datum ist eine wichtige Information in der Programmierung, da es für viele Anwendungsfälle benötigt wird, wie beispielsweise die Verwaltung von Zeitstempeln, die Berechnung von Vergangenheit oder Zukunft oder die Anzeige von aktuellen Daten in einer Anwendung.

##Wie man das aktuelle Datum in Ruby erhält

In Ruby gibt es verschiedene Methoden, um das aktuelle Datum zu erhalten. Eine Möglichkeit ist die Verwendung der `Date` Klasse.

```ruby
heute = Date.today
puts heute
# Ausgabe: 2021-03-17
```

Eine weitere Möglichkeit besteht darin, `Time` zu verwenden, um das aktuelle Datum und die aktuelle Uhrzeit zu erhalten.

```ruby
jetzt = Time.now
puts jetzt
# Ausgabe: 2021-03-17 11:30:00 +0100
```

Für eine genauere Steuerung des Datumsformats kann die `strftime` Methode verwendet werden.

```ruby
puts jetzt.strftime("%d.%m.%Y %H:%M")
# Ausgabe: 17.03.2021 11:30
```

##Tiefere Einblicke

Wenn wir uns das Datum genauer ansehen möchten, können wir uns verschiedene Eigenschaften wie den Tag, den Monat oder das Jahr ausgeben lassen.

```ruby
puts heute.day # Tag
# Ausgabe: 17
puts heute.month # Monat
# Ausgabe: 3
puts heute.year # Jahr
# Ausgabe: 2021
```

Außerdem gibt es in Ruby auch die Möglichkeit, Datum-Berechnungen durchzuführen, wie zum Beispiel das Hinzufügen oder Subtrahieren von Tagen.

```ruby
puts heute + 7 # Eine Woche hinzufügen
# Ausgabe: 2021-03-24
puts heute - 3 # Drei Tage subtrahieren
# Ausgabe: 2021-03-14
```

Für weitere Informationen zu den verschiedenen Methoden und Eigenschaften, die mit Datum und Zeit in Ruby verwendet werden können, empfehle ich die offizielle Dokumentation.

##Siehe auch

- [Offizielle Ruby Dokumentation zu Date und Time](https://ruby-doc.org/stdlib-2.7.2/libdoc/date/rdoc/Date.html)
- [Artikel über Datum-Manipulation in Ruby](https://www.rubyguides.com/2019/02/ruby-date/)
- [Video-Tutorial zur Verwendung von Date und Time in Ruby](https://www.youtube.com/watch?v=Q-1Geb8zEqk)