---
title:                "Arbeiten mit JSON"
date:                  2024-01-19
html_title:           "Arduino: Arbeiten mit JSON"
simple_title:         "Arbeiten mit JSON"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/ruby/working-with-json.md"
---

{{< edit_this_page >}}

## Was & Warum?

JSON (JavaScript Object Notation) ist ein Format zum Datenaustausch. Ruby-Entwickler nutzen es häufig, um Daten zwischen Server und Client zu senden oder um Konfigurationsdateien zu lesen und zu schreiben.

## How to:

```Ruby
# Zum Parsen eines JSON-Strings:
require 'json'

json_string = '{"name": "Max", "alter": 28, "sprachen": ["Deutsch", "Englisch"]}'
parsed_data = JSON.parse(json_string)
puts parsed_data["name"] # Ausgabe: Max

# Zum Erzeugen eines JSON-Strings aus einem Ruby-Hash:
ruby_hash = { name: "Anna", alter: 22, sprachen: ["Deutsch", "Spanisch"] }
json_output = ruby_hash.to_json
puts json_output # Ausgabe: {"name":"Anna","alter":22,"sprachen":["Deutsch","Spanisch"]}
```

## Deep Dive

Erstmals 2001 spezifiziert, hat sich JSON zu einem der zentralen Standards für die Datenübertragung im Web entwickelt. Es ist leichter und schneller zu parsen als XML. Ruby bietet Standardbibliotheken wie `json` für das einfache Parsen und Generieren von JSON. Trotz Alternativen wie YAML für Konfigurationen oder MessagePack für Binärkommunikation bleibt JSON vor allem in API-Interaktionen relevant. Ruby's JSON-Bibliothek ist in C implementiert, was eine effiziente Verarbeitung ermöglicht.

## See Also

- Ruby’s JSON Library Dokumentation: https://ruby-doc.org/stdlib/libdoc/json/rdoc/JSON.html
- JSON Spezifikation: https://www.json.org/json-de.html
- Ein tiefergehender Vergleich von JSON und XML: https://www.w3schools.com/js/js_json_xml.asp
