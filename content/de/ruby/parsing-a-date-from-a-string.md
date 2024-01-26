---
title:                "Datum aus einem String parsen"
date:                  2024-01-20T15:38:06.043009-07:00
html_title:           "Arduino: Datum aus einem String parsen"
simple_title:         "Datum aus einem String parsen"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/ruby/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?
Datum-Parsing verwandelt Text in ein Datum-Objekt. Das wird gemacht, um Datumsangaben zu vergleichen, zu speichern oder zu manipulieren.

## How to:
```Ruby
require 'date'

# Beispiel: Datum aus einem String parsen
datum_string = '2023-04-05'
datum_objekt = Date.parse(datum_string)
puts datum_objekt.strftime('%d.%m.%Y') # Gibt aus: "05.04.2023"

# Falls das Format bekannt ist:
datum_objekt = Date.strptime(datum_string, '%Y-%m-%d')
puts datum_objekt.strftime('%d.%m.%Y') # Gibt aus: "05.04.2023"
```
Sample Output:
```
05.04.2023
05.04.2023
```

## Deep Dive:
Datums-Parsing in Ruby verwendet die 'date' Bibliothek. Vor 'date' nutzte man andere Wege, z.B. Time.parse. Die 'date' Bibliothek ist jedoch besser, weil sie mehr Formate automatisch erkennt und einfacher ist.

Es gibt Alternativen wie 'chronic', die noch flexiblere Datums-Parsing-Methoden bieten. 'chronic' versteht natürlichsprachliche Datenangaben wie "nächsten Dienstag".

Bei der Umsetzung ist es wichtig zu wissen, dass `Date.parse` automatisch Formate erkennt, was zu Fehlern führen kann, wenn das Datum-Format variabel ist. `Date.strptime` vermeidet das, indem es ein festes Format verlangt.

## See Also:
- Info über 'chronic': [chronic - GitHub](https://github.com/mojombo/chronic)
