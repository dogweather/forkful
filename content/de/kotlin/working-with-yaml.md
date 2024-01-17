---
title:                "Arbeiten mit yaml"
html_title:           "Kotlin: Arbeiten mit yaml"
simple_title:         "Arbeiten mit yaml"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/working-with-yaml.md"
---

{{< edit_this_page >}}

## Was & Warum?
YAML ist eine einfache, menschenlesbare Datenformatierungssprache, die häufig von Programmierern verwendet wird, um Konfigurationsdateien zu erstellen. Es ist eine großartige Alternative zu anderen Formaten wie JSON, da es weniger zeitaufwändig und weniger fehleranfällig ist.

## So geht's:
```Kotlin
// Erstellen einer YAML-Konfigurationsdatei
val config = """
    name: John
    age: 25
    hobbies:
        - hiking
        - reading
    """.trimIndent()

// Lesen einer YAML-Konfigurationsdatei
val name = yaml["name"] // John
val age = yaml["age"] // 25
val hobbies = yaml["hobbies"] // [hiking, reading]
```

## Tiefere Einblicke:
YAML wurde ursprünglich 2001 von Clark Evans für das Ruby-on-Rails-Projekt entwickelt. Es ist eine Alternative zu XML und anderen Formaten, die es ermöglicht, Daten in einer lesbareren und leichter zu schreibenden Art und Weise zu strukturieren. YAML ist auch bekannt für seine einfache Syntax und die Möglichkeit, Kommentare hinzuzufügen. Es ist in vielen Programmiersprachen verfügbar, einschließlich Java, Python und natürlich Kotlin.

## Siehe auch:
- [YAML.org](https://yaml.org/)
- [Kotlin Dokumentation](https://kotlinlang.org/docs/)
- [YamlBeans Bibliothek für Kotlin](https://github.com/EsotericSoftware/yamlbeans)