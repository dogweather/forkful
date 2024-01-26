---
title:                "Berechnung eines zukünftigen oder vergangenen Datums"
date:                  2024-01-20T17:32:12.679439-07:00
model:                 gpt-4-1106-preview
simple_title:         "Berechnung eines zukünftigen oder vergangenen Datums"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/ruby/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Was & Warum?
Eine Datumsberechnung in der Zukunft oder Vergangenheit ermöglicht es, mit Zeitintervallen zu arbeiten - zum Beispiel, um Fristen zu setzen oder Ereignisse zu planen. Programmierer nutzen dies häufig für Features wie Erinnerungen, Ablaufverfolgungen und Terminplanungen.

## How to:
Mit Ruby ist es einfach:

```Ruby
require 'date'

# heutiges Datum
today = Date.today
puts "Heute ist: #{today}"

# 10 Tage in der Zukunft
future_date = today + 10
puts "In 10 Tagen: #{future_date}"

# 20 Tage in der Vergangenheit
past_date = today - 20
puts "Vor 20 Tagen: #{past_date}"
```

Beispielausgabe:

```
Heute ist: 2023-04-05
In 10 Tagen: 2023-04-15
Vor 20 Tagen: 2023-03-16
```

## Deep Dive:
Früher, in den Zeiten vor einfachen Bibliotheken, mussten Entwickler komplexe Algorithmen selbst schreiben, um mit Datums- und Zeitangaben zu hantieren. In Ruby löst die Standardbibliothek `Date` diese Probleme. Es respektiert sogar Schaltjahre beim Addieren von Tagen zu einem Datum.

Alternativ zu `Date` gibt es auch `Time` für Zeitstempel inklusive Uhrzeit und `DateTime` für eine Kombination beider. Bibliotheken wie `ActiveSupport` bieten noch mehr Flexibilität, z.B. mit `3.days.from_now` in Rails-Projekten.

Details: Ruby's `Date` arbeitet mit dem Gregorianischen Kalender, und `+` bzw. `-` Methoden addieren oder subtrahieren Tage als ganze Zahlen. Es wird im Hintergrund die Julian Day Number verwendet, was das Rechnen mit größeren Zeitspannen präzise macht.

## See Also:
Für weiterführende Informationen und fortgeschrittenere Themen, sieh dir diese Ressourcen an:

- Ruby's Standardbibliothek: [Date](https://ruby-doc.org/stdlib-3.0.0/libdoc/date/rdoc/Date.html)
- ActiveSupport's Zeitrechenmethoden: [ActiveSupport Core Extensions](https://guides.rubyonrails.org/active_support_core_extensions.html#extensions-to-date)
