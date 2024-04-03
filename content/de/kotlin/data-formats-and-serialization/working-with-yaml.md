---
date: 2024-01-19
description: "YAML ist ein Format f\xFCr Daten, das auf Menschen ausgerichtet ist.\
  \ Programmierer nutzen es, weil es einfach zu lesen und zu schreiben ist und sich\
  \ gut f\xFCr\u2026"
lastmod: '2024-03-13T22:44:53.866576-06:00'
model: unknown
summary: "YAML ist ein Format f\xFCr Daten, das auf Menschen ausgerichtet ist."
title: Arbeiten mit YAML
weight: 41
---

## Was & Warum?
YAML ist ein Format für Daten, das auf Menschen ausgerichtet ist. Programmierer nutzen es, weil es einfach zu lesen und zu schreiben ist und sich gut für Konfigurationsdateien oder Datenmodellierung eignet.

## How to:
In Kotlin kannst du YAML mit der SnakeYAML-Bibliothek verarbeiten. Hier ein Beispiel, um eine YAML-Datei zu lesen:

```Kotlin
import org.yaml.snakeyaml.Yaml
import java.io.FileReader

fun main() {
    val yaml = Yaml()
    val reader = FileReader("config.yaml")

    // YAML-Inhalt als Map lesen
    val data: Map<String, Any> = yaml.load(reader)
    println(data)
}
```

Nimm an, `config.yaml` sieht so aus:

```YAML
name: Max Mustermann
age: 30
```

Die Ausgabe wäre dann:

```Kotlin
{name=Max Mustermann, age=30}
```

## Deep Dive:
YAML entstand Anfang der 2000er Jahre als einfachere Alternative zu XML. Im Vergleich zu JSON erlaubt YAML Kommentare und ist durch Einrückungen menschenfreundlicher lesbar. Intern wird YAML in der Regel in JSON oder in eine Map konvertiert, was die Verarbeitung erleichtert. Alternativen zu YAML könnten JSON oder TOML sein, je nachdem, welche Anforderungen zu erfüllen sind.

## See Also:
- SnakeYAML GitHub Repository: https://github.com/asomov/snakeyaml
- YAML offizielle Seite: https://yaml.org/
- Kotlin Dokumentation: https://kotlinlang.org/docs/home.html
