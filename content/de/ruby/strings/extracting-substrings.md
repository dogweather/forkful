---
date: 2024-01-20 17:46:43.797135-07:00
description: "Das Extrahieren von Teilzeichenketten (Substrings) hei\xDFt, spezifische\
  \ Teile aus einem String herauszuschnippeln. Programmierer machen das, um bestimmte\u2026"
lastmod: '2024-03-13T22:44:54.387152-06:00'
model: gpt-4-1106-preview
summary: "Das Extrahieren von Teilzeichenketten (Substrings) hei\xDFt, spezifische\
  \ Teile aus einem String herauszuschnippeln. Programmierer machen das, um bestimmte\u2026"
title: Teilstrings extrahieren
---

{{< edit_this_page >}}

## Was & Warum?

Das Extrahieren von Teilzeichenketten (Substrings) heißt, spezifische Teile aus einem String herauszuschnippeln. Programmierer machen das, um bestimmte Daten zu isolieren oder zu manipulieren, zum Beispiel in Textanalysen oder beim Daten-Scrubbing.

## Anleitung:

```Ruby
# Einen String initialisieren
satz = "Ich bin ein Ruby-Entwickler!"

# Extrahieren mit der slice-Methode
teil1 = satz.slice(0, 3)          # Ergibt "Ich"
teil2 = satz.slice(4, 3)          # Ergibt "bin"
teil3 = satz[11, 5]               # Alternativer Zugriff, ergibt "Ruby"

# Extrahieren mit Bereichsangaben
teil4 = satz[16..-1]              # Ergibt "Entwickler!"

# Ausgabe
puts teil1
puts teil2
puts teil3
puts teil4
```

Erwartete Ausgabe:

```
Ich
bin
Ruby
Entwickler!
```

## Vertiefung:

Bevor Programmiersprachen moderne Strings und Hilfsmethoden boten, mussten Programmierer manuell Zeichen durchlaufen und kopieren. Heute bieten Sprachen wie Ruby mächtige Methoden wie `slice` und Bereichsangaben (`Range`), um Substrings effizient zu extrahieren.

Alternativen in Ruby sind `[]`, `slice`, `slice!` (ändert den Originalstring) und Reguläre Ausdrücke. Letztere sind besonders nützlich, wenn es um komplexe Suchmuster geht.

Unter der Haube verwendet Ruby Objekte und Methoden, um mit Textdaten umzugehen. Die Implementierung solcher Features kann in C geschrieben sein und nutzt meist effiziente Algorithmen für Textmanipulation.

## Siehe auch:

- Ruby-Dokumentation zu Strings: [Ruby-Doc String](https://ruby-doc.org/core-2.7.0/String.html)
- Ruby API-Dokumentation: [Ruby API](https://rubyapi.org/2.7/o/string#method-i-slice)
- Tutorial zu regulären Ausdrücken in Ruby: [Ruby Regexp](https://www.rubyguides.com/2015/06/ruby-regex/)
