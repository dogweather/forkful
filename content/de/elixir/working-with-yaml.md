---
title:                "Arbeiten mit YAML"
html_title:           "Elixir: Arbeiten mit YAML"
simple_title:         "Arbeiten mit YAML"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/working-with-yaml.md"
---

{{< edit_this_page >}}

## Was ist YAML & Warum benutzen es Programmierer?

YAML ist eine plattformübergreifende Sprache zur Darstellung von Daten in einem menschenlesbaren Format. Programmierer nutzen YAML, um Konfigurationsdateien und andere strukturierte Daten zu erstellen, die leicht zu lesen und zu bearbeiten sind.

## Wie man es benutzt:

Benutze die `Elixir YAML` Bibliothek, um Daten in YAML-Format zu konvertieren und umgekehrt.

```Elixir
# YAML zu Elixir
YAML.decode("""
  - name: Max
    age: 25
  - name: Anna
    age: 30
""")
# => [%{"name" => "Max", "age" => 25}, %{"name" => "Anna", "age" => 30}]

# Elixir zu YAML
YAML.encode([
  %{"name" => "Max", "age" => 25},
  %{"name" => "Anna", "age" => 30}
])
# => - name: Max
#    age: 25
#  - name: Anna
#    age: 30
```

## Tiefere Einblicke:

YAML wurde ursprünglich als Alternative zu XML entwickelt und zeichnet sich durch eine einfachere und intuitivere Syntax aus. Es wird von vielen Programmiersprachen, einschließlich Elixir, unterstützt und eignet sich ideal für das Erstellen von Konfigurationsdateien, Datenübertragung und Speichern von strukturierten Daten.

Einige Programmierer bevorzugen möglicherweise JSON oder TOML gegenüber YAML. JSON hat eine sehr ähnliche Syntax zu YAML, aber es werden unterschiedliche Datentypen unterstützt. TOML ist eine noch jüngere Alternative, die sich durch eine strengere Syntax auszeichnet.

Die Implementation von YAML in Elixir basiert auf der `libyaml` Bibliothek. Diese Bibliothek ermöglicht es Elixir, Daten in YAML-Format zu lesen und zu schreiben.

## Siehe auch:

* [`YAML` Elixir Bibliothek Dokumentation](https://github.com/jeremyong/yaml_elixir)
* [YAML Spezifikation](https://yaml.org/spec/)
* [Offizielle YAML Website](https://yaml.org/)