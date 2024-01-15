---
title:                "Arbeiten mit Yaml"
html_title:           "Kotlin: Arbeiten mit Yaml"
simple_title:         "Arbeiten mit Yaml"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/working-with-yaml.md"
---

{{< edit_this_page >}}

## Warum

YAML ist eine einfache und intuitive Möglichkeit, Daten zu speichern und zu übertragen. Es kann in vielen Bereichen verwendet werden, wie z.B. Konfigurationsdateien, REST APIs und Datenbanken. Wenn du also in der Programmierung tätig bist, ist es wichtig zu verstehen, wie man mit YAML arbeitet.

## Wie man mit YAML arbeitet

Das Erstellen von YAML-Dateien ist einfach und kann mit jedem Texteditor erfolgen. Hier ein Beispiel, wie man eine Liste mit Namen und Alter erstellen kann:

```Kotlin
- name: Max
  age: 25

- name: Lisa
  age: 30
```

Man kann auch komplexe Datenstrukturen wie verschachtelte Listen und Objekte erstellen:

```Kotlin
- name: John
  age: 35
  hobbies:
    - hiking
    - cooking
  address:
    street: Main Street
    city: Berlin
```

Um auf diese Daten in einem Kotlin-Programm zuzugreifen, kann man die Bibliothek "Snakeyaml" verwenden. Hier ein Beispiel, wie man die Daten aus der obigen YAML-Datei auslesen kann:

```Kotlin
import org.yaml.snakeyaml.Yaml

val yaml = Yaml()
val data = yaml.load("example.yaml") // Lade YAML-Datei
val name = data[0]["name"] // John
val age = data[0]["age"] // 35
val hobbies = data[0]["hobbies"] // [hiking, cooking]
val address = data[0]["address"] // {street=Main Street, city=Berlin}
```

## Tiefergehende Informationen

Wenn du tiefer in die Arbeit mit YAML einsteigen möchtest, gibt es einige wichtige Konzepte zu beachten. Zum Beispiel gibt es verschiedene Datentypen in YAML wie Strings, Zahlen, Listen und Objekte. Auch die Einrückung ist wichtig, um verschachtelte Datenstrukturen korrekt darzustellen.

Es ist auch möglich, YAML-Dateien mit Kotlin nativ zu erstellen und zu bearbeiten, ohne eine externe Bibliothek zu verwenden. Dafür bieten die Standardbibliotheken "kotlinx-serialization-yaml" und "kotlinx-yaml" eine einfache Möglichkeit, YAML-Daten in Kotlin-Objekte zu konvertieren und umgekehrt.

Es gibt auch fortgeschrittenere Themen wie das Verwenden von Anker und Aliase, um wiederkehrende Strukturen in einer Datei zu reduzieren, oder das Formatieren von YAML-Dateien mit speziellen Optionen.

## Siehe auch

- [Kotlin-Referenz zur Bibliothek "Snakeyaml"](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.properties/-script.html)
- [Einführung in die Nutzung von YAML in Kotlin](https://blog.suitcasecode.com/working-with-yaml-in-kotlin/)
- [Überblick über die Standardbibliotheken für die Verarbeitung von YAML in Kotlin](https://kotlinlang.org/api/latest/kotlinx-serialization-kotlinx-serialization-yaml/)