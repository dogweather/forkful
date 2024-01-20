---
title:                "Einen Datum aus einem String parsen"
html_title:           "Elixir: Einen Datum aus einem String parsen"
simple_title:         "Einen Datum aus einem String parsen"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/ruby/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Parsen eines Datums aus einem String in Ruby bedeutet, diesen String in ein Datum-Objekt umzuwandeln. Programmierer tun dies, um mit den Daten in Formaten zu arbeiten, die mit Datums- und Zeitoperationen kompatibel sind.

## So geht's:

Mit der Ruby's built-in Methode `parse` von `Date` Klasse können wir ganz einfach ein Datum aus einem String parsen:

```Ruby
require 'date'
datum_string = "2022-01-01"
datum = Date.parse(datum_string)
puts datum
```
Die Ausgabe wird "2022-01-01" sein.

## Tiefer Eintauchen

Parsen eines Datums hat eine lange Geschichte. Früher mussten Programmierer teils komplexe Algorithmen schreiben, um dies zu erreichen. Heute erleichtert Ruby's eingebaute ‘Date’ Klasse diesen Vorgang.

Es gibt auch Alternativen zur Methode `parse`. Einige davon sind `strptime` oder `iso8601`. Der Unterschied liegt darin, dass `strptime` und `iso8601` ein bestimmtes Format erwarten, während `parse` verschiedene Formate behandelt.

Bei der Umsetzung dieser Methode ist darauf zu achten, dass `parse` eine mögliche Ausnahme wie "ArgumentError" wirft, wenn der String nicht in ein gültiges Datum umgewandelt werden kann. Daher ist es praktisch, den Prozess in einer "begin-rescue"-Struktur zu verpacken, um solche Probleme zu behandeln.

## Siehe auch:

1. [Ruby's Date documentation](https://ruby-doc.org/stdlib-3.0.0/libdoc/date/rdoc/Date.html)
2. [Ruby's time handling (stackoverflow)](https://stackoverflow.com/questions/3700110/how-do-i-use-date-strptime-with-a-format-from-datetime-to-time)
3. [More on parsing dates (tutorialspoint)](https://www.tutorialspoint.com/ruby/ruby_date_time.htm)