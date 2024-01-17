---
title:                "Arbeiten mit YAML"
html_title:           "Java: Arbeiten mit YAML"
simple_title:         "Arbeiten mit YAML"
programming_language: "Java"
category:             "Java"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/java/working-with-yaml.md"
---

{{< edit_this_page >}}

## Was ist das & Warum?

Bei der Arbeit mit Java müssen wir oft mit Konfigurationsdateien arbeiten, um unsere Anwendungen zu verwalten. Eine sehr beliebte Art, Konfigurationsdateien zu erstellen, ist die Verwendung von YAML, einer einfachen und menschenlesbaren Datenformatierungssprache. YAML ist besonders hilfreich, da es flexibel, benutzerfreundlich und plattformunabhängig ist. Es ist auch bekannt für seine einfache Syntax und Flexibilität bei der Integration mit anderen Programmiersprachen.

## Wie geht das?

Um mit YAML in Java zu arbeiten, müssen wir zuerst die YAML-Library in unserem Projekt importieren. Anschließend können wir ganz einfach YAML-Dateien in unserem Code verarbeiten.

### Beispiel:

```Java
Yaml yaml = new Yaml();

// Lese eine YAML-Datei und speichere die Daten in einem Map-Objekt
Map<String, Object> configData = yaml.load(new FileInputStream("config.yml"));
``` 
Dieser Code liest eine YAML-Datei mit dem Namen "config.yml" und speichert die Daten in einem Map-Objekt, das wir dann in unserem Code verwenden können.

### Beispiele zur Verarbeitung von Daten:

Um auf einzelne Werte in unserem Map-Objekt zuzugreifen, können wir einfach den Schlüssel des entsprechenden Werts verwenden:

```Java
// Zugriff auf Wert "port" in unserem Map-Objekt
Integer port = (Integer) configData.get("port");
```

Wir können auch komplexe Datenstrukturen wie Listen und verschachtelte Maps verarbeiten:

```Java
// Zugriff auf Titel des ersten Videos in unserer Liste
String videoTitle = (String) ((Map<String, Object>) configData.get("videos")).get("1").get("title");
```

## Tiefere Einblicke

YAML wurde ursprünglich als Teil der Programmiersprache Perl entwickelt, um die Lesbarkeit von Konfigurationsdateien zu verbessern. Mittlerweile wird es jedoch von vielen anderen Programmiersprachen unterstützt. Als Alternative zu YAML gibt es auch andere Formate wie JSON oder XML, die jedoch in der Regel weniger benutzerfreundlich sind.

Um YAML in Java zu verwenden, können wir auch die SnakeYAML-Library verwenden, die zusätzliche Funktionen bietet, um die Arbeit mit YAML noch einfacher zu gestalten.

## Siehe auch

- [YAML-Website](https://yaml.org/)
- [Offizielle Dokumentation für SnakeYAML](https://bitbucket.org/asomov/snakeyaml/wiki/Documentation)