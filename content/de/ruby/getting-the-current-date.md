---
title:                "Aktuelles Datum abrufen"
date:                  2024-01-20T15:16:03.919373-07:00
simple_title:         "Aktuelles Datum abrufen"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/ruby/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Abrufen des aktuellen Datums in Ruby ist ein Weg, das heutige Datum zu bekommen. Programmierer nutzen das für Features wie Datumsstempel, Zeitleisten oder einfach nur, um zu wissen, welcher Tag heute ist.

## So geht's:
```Ruby
require 'date'

# Das aktuelle Datum erhalten
heute = Date.today
puts "Heute ist der #{heute}"

# Ausgabe
# Heute ist der 2023-04-12
```

## Deep Dive
Ruby bietet mit der Standardbibliothek 'date' einfache Möglichkeiten für die Arbeit mit Daten. Früher mussten Entwickler oft externe Bibliotheken verwenden oder eigenes Code-Geraffel schreiben, um mit Daten zu hantieren.

Es gibt Alternativen wie 'Time.now' oder 'DateTime.now', aber 'Date.today' ist oft die beste Wahl, wenn man nur das Datum ohne Zeitangaben braucht. Unter der Haube verwendet Ruby Methoden, die plattformunabhängig sind, also keine Sorgen, ob Ihr Code auf verschiedenen Systemen gleich läuft.

## Siehe auch
- Ruby-Dokumentation für die 'Time'-Klasse: [Ruby Time Class](https://ruby-doc.org/core/Time.html)
