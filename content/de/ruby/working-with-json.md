---
title:                "Arbeiten mit JSON"
html_title:           "Ruby: Arbeiten mit JSON"
simple_title:         "Arbeiten mit JSON"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/ruby/working-with-json.md"
---

{{< edit_this_page >}}

# Was & Warum?

JSON (JavaScript Object Notation) ist ein Format zur Speicherung und Übertragung von Daten. Es wird häufig verwendet, um Daten zwischen verschiedenen Anwendungen auszutauschen oder um Daten in einer Datenbank zu speichern. Programmierer nutzen JSON, um Daten in einer leicht lesbaren und verständlichen Struktur zu übermitteln.

## Wie geht's?

Um mit JSON in Ruby zu arbeiten, muss zunächst die `json` Bibliothek importiert werden. Dann können Daten mit der `JSON.parse()` Methode von einer JSON-Zeichenkette in ein Ruby-Objekt und mit der `JSON.generate()` Methode vom Ruby-Objekt in eine JSON-Zeichenkette umgewandelt werden.

```Ruby
require "json"

# JSON in Ruby-Objekt umwandeln
json_string = '{"name":"Max","age":25,"country":"Deutschland"}'
ruby_object = JSON.parse(json_string)

# Ruby-Objekt in JSON-Zeichenkette umwandeln
ruby_object = { name: "Lisa", age: 28, country: "USA" }
json_string = JSON.generate(ruby_object)
```
Das Ergebnis der `JSON.parse()` Methode ist ein mit Hashes und Arrays strukturiertes Ruby-Objekt. Das Ergebnis der `JSON.generate()` Methode ist eine JSON-Zeichenkette im gleichen Format wie die Eingabe.

## Tiefgehende Einblicke

JSON wurde ursprünglich von Douglas Crockford entwickelt und 2002 veröffentlicht. Es ist mittlerweile ein populäres Format zur Datenübermittlung und Konfiguration in der Webentwicklung geworden. Alternativen zu JSON sind beispielsweise XML, YAML und CSV. In Ruby gibt es auch die Möglichkeit, JSON mit der `Hash#to_json` Methode zu konvertieren.

Weitere Informationen zu JSON in Ruby und mögliche Anwendungen finden sich in der offiziellen Dokumentation [hier] (https://ruby-doc.org/stdlib-2.6.3/libdoc/json/rdoc/JSON.html) und [hier] (https://www.json.org/).

## Siehe auch

- [JSON auf GitHub] (https://github.com/json)
- [Einführung in JSON] (https://www.digitalocean.com/community/tutorials/an-introduction-to-json) von DigitalOcean
- [JSON und Ruby] (https://pragmaticstudio.com/tutorials/using-json-data-in-ruby) von Pragmatic Studio