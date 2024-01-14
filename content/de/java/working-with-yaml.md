---
title:                "Java: Arbeiten mit YAML"
simple_title:         "Arbeiten mit YAML"
programming_language: "Java"
category:             "Java"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/java/working-with-yaml.md"
---

{{< edit_this_page >}}

# Warum YAML?

In der Programmierung gibt es zahlreiche Möglichkeiten, Daten zu speichern und zu verarbeiten. Eine davon ist YAML, eine menschenlesbare Datenstruktur, die besonders für die Konfiguration von Programmen geeignet ist. In diesem Blog-Beitrag werden wir genauer betrachten, warum YAML eine nützliche Wahl für Programmierer ist.

## Wie man mit YAML arbeitet

YAML Syntax basiert auf Einrückungen und ermöglicht somit eine übersichtliche Darstellung von Daten, ohne die Verwendung von Sonderzeichen wie Klammern. Um mit YAML in Java zu arbeiten, gibt es verschiedene Bibliotheken, die uns dabei helfen, YAML-Dateien auszulesen und zu schreiben.

Ein Beispiel mit der Bibliothek "SnakeYAML":

```Java
Yaml yaml = new Yaml();
Map<String, Object> data = yaml.load(new FileInputStream("config.yml")); // YAMl-Datei einlesen
System.out.println(data.get("username")); // Wert der Property "username" ausgeben
```

Die Ausgabe wäre dann der Benutzername aus der YAML-Datei. Mit der Bibliothek lassen sich Daten natürlich auch in eine YAML-Datei schreiben.

## Tiefer Einblick in YAML

YAML besteht aus verschiedenen Datentypen, wie zum Beispiel Boolean, Integer, String, Arrays oder Maps. Diese können beliebig verschachtelt werden, um komplexe Datenstrukturen darzustellen. Auch Kommentare sind möglich, um die Lesbarkeit von YAML-Dateien noch weiter zu verbessern.

Es gibt auch die Möglichkeit, Variablen innerhalb von YAML-Dateien zu definieren und diese in anderen Teilen der Datei zu nutzen. Dadurch können zum Beispiel häufig genutzte Werte zentral an einer Stelle definiert werden und müssen nicht in jeder Zeile wiederholt werden.

Ein weiterer Vorteil von YAML ist, dass die Datenstruktur nicht an eine bestimmte Programmiersprache gebunden ist. Somit können die YAML-Dateien auch von anderen Programmiersprachen, wie Python oder Ruby, problemlos verarbeitet werden.

# Siehe auch

- [YAML.org](https://yaml.org/)
- [SnakeYAML Dokumentation](https://bitbucket.org/asomov/snakeyaml/src/default/doc/index.html)
- [YAML in 5 Minuten erklärt](https://learnxinyminutes.com/docs/yaml/)