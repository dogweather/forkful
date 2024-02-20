---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:24:02.491997-07:00
description: "JSON (JavaScript Object Notation) ist ein leichtgewichtiges Daten-Austauschformat,\
  \ das vor allem in Webanwendungen f\xFCr den Datenaustausch zwischen Clients\u2026"
lastmod: 2024-02-19 22:05:13.364815
model: gpt-4-0125-preview
summary: "JSON (JavaScript Object Notation) ist ein leichtgewichtiges Daten-Austauschformat,\
  \ das vor allem in Webanwendungen f\xFCr den Datenaustausch zwischen Clients\u2026"
title: Arbeiten mit JSON
---

{{< edit_this_page >}}

## Was & Warum?

JSON (JavaScript Object Notation) ist ein leichtgewichtiges Daten-Austauschformat, das vor allem in Webanwendungen für den Datenaustausch zwischen Clients und Servern verbreitet ist. Programmierer arbeiten mit JSON in Ruby, um Daten von externen Quellen zu parsen oder Daten für API-Antworten zu formatieren, wobei sie seine menschenlesbare Struktur für eine einfache Datenmanipulation und -speicherung nutzen.

## Wie:

Ruby bietet mit seiner Standardbibliothek nahtlose Möglichkeiten zum Parsen und Generieren von JSON. Das primäre Modul für diese Operationen ist `json`, das leicht in jede Ruby-Anwendung integriert werden kann.

### JSON parsen:

Um einen JSON-String in ein Ruby-Hash zu konvertieren, können Sie die Methode `JSON.parse` verwenden.

```ruby
require 'json'

json_string = '{"name": "John Doe", "age": 30, "city": "New York"}'
ruby_hash = JSON.parse(json_string)

puts ruby_hash
# Ausgabe: {"name"=>"John Doe", "age"=>30, "city"=>"New York"}
```

### JSON generieren:

Umgekehrt, um ein Ruby-Hash in einen JSON-String zu konvertieren, verwenden Sie die Methode `JSON.generate` oder die Methode `to_json`, die auf Ruby-Objekte verfügbar ist, sobald die `json`-Bibliothek eingebunden ist.

```ruby
require 'json'

ruby_hash = { name: "Jane Doe", age: 25, city: "Los Angeles" }
json_string = ruby_hash.to_json

puts json_string
# Ausgabe: {"name":"Jane Doe","age":25,"city":"Los Angeles"}
```

### Drittanbieter-Bibliotheken:

Während Rubys Standardbibliothek die grundlegende JSON-Verarbeitung abdeckt, setzen viele Projekte auf Drittanbieter-Bibliotheken für verbesserte Funktionalität und Leistung. Eine beliebte Wahl ist `Oj` (Optimized JSON).

#### Parsen mit Oj:

```ruby
require 'oj'

json_string = '{"name": "Alex", "age": 40, "city": "Chicago"}'
ruby_hash = Oj.load(json_string)

puts ruby_hash
# Ausgabe: {"name"=>"Alex", "age"=>40, "city"=>"Chicago"}
```

#### Generieren mit Oj:

Oj bietet auch eine schnelle Möglichkeit, JSON aus Ruby-Objekten zu generieren:

```ruby
require 'oj'

ruby_hash = { name: "Samantha", age: 35, city: "Miami" }
json_string = Oj.dump(ruby_hash)

puts json_string
# Ausgabe: {"name":"Samantha","age":35,"city":"Miami"}
```
Diese Beispiele veranschaulichen die unkomplizierte Natur der Arbeit mit JSON in Ruby, was es für Aufgaben von einfachen Datenmanipulationen bis hin zu komplexen API-Kommunikationen zugänglich macht.
