---
title:                "Arbeiten mit YAML"
html_title:           "Bash: Arbeiten mit YAML"
simple_title:         "Arbeiten mit YAML"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/working-with-yaml.md"
---

{{< edit_this_page >}}

# YAML in Gleam nutzen: Ein praktischer Guide

## Was & Warum?
YAML ist ein Datenformat für Konfigurationsdateien und Datenaustausch. Programmierer nutzen es wegen seiner Lesbarkeit und Einfachheit für Einstellungen, Daten speichern und teilen.

## Anleitung:
Leider gibt es in Gleam noch keine eingebaute Bibliothek für YAML. Wir können jedoch eine externe Library nutzen. Hier ein Beispiel, wie man YAML in Gleam liest und schreibt:

```gleam
// externes YAML-Paket importieren, vorher hinzufügen und installieren
import yaml/gleam_yaml

// YAML String in Gleam-Datenstruktur umwandeln
let data = yaml.from_str("
person:
  name: Kai
  age: 32
")

// Entsprechendes Gleam-Modell prüfen
case data {
  Ok(value) -> value
  Error(_) -> panic("Invalid YAML data")
}

// Gleam-Struktur in YAML String umwandeln
let output = yaml.to_string(person)
```

## Tiefgang:
YAML tritt als JSON-Alternative auf, debütierte 2001. JSON und XML sind Alternativen. YAMLs Design setzt auf Einrückungen für Struktur, vermeidet Klammer-Syntax. Aufmerksamkeit bei der Implementierung gilt der richtigen Einrückung und Typkonvertierung.

## Siehe Auch:
- YAML official website: [yaml.org](https://yaml.org)
- YAML Wikipedia-Artikel: [de.wikipedia.org](https://de.wikipedia.org/wiki/YAML)
