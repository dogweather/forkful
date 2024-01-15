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

## Warum

Warum sollte man sich mit YAML beschäftigen? Ganz einfach: YAML ist eine strukturierte Datenformatierungssprache, die es ermöglicht, Daten in einem einfach zu lesenden und zu schreibenden Format zu organisieren. Sie wird häufig in der Softwareentwicklung verwendet, insbesondere bei der Konfiguration von Anwendungen.

## Wie geht's?

In dieser Anleitung werden wir uns ansehen, wie man YAML in Java verwenden kann. Wir werden einige grundlegende Beispiele durchgehen und die Ausgabe betrachten. Zuerst müssen wir jedoch sicherstellen, dass wir die richtigen Abhängigkeiten in unserem Projekt haben. Wir benötigen die LibYAML Bibliothek, um die YAML-Dateien zu verarbeiten.

```Java
// Import für die LibYAML Bibliothek
import org.yaml.snakeyaml.*;
```

Nun können wir eine YAML-Datei analysieren und die Daten ausgeben:

```Java
// Erstellen eines Yaml-Objekts
Yaml yaml = new Yaml();

// Lade die Datei und speichere sie in einer HashMap
HashMap data = (HashMap) yaml.load(new FileInputStream("example.yaml"));

// Gib den Inhalt der Datei aus
System.out.println(data);
```

Die Ausgabe sieht wie folgt aus:

```Java
// Ausgabe
{key1=value1, key2=value2, key3=value3}
```

Wir können auch spezifische Werte aus der Datei abrufen, indem wir den entsprechenden Schlüssel verwenden:

```Java
// Ausgabe des Werts von key1
System.out.println(data.get("key1"));
```

Die Ausgabe wäre dann `value1`. Auf diese Weise können wir die Daten aus der YAML-Datei in unsere Java-Anwendung integrieren.

## Tiefergehende Informationen

In diesem Abschnitt werden wir etwas genauer darauf eingehen, wie man mit YAML in Java arbeitet und einige fortgeschrittene Funktionen betrachten.

### YamlReader und YamlWriter

Anstatt die `load()`-Methode von Yaml zu verwenden, können wir auch YamlReader und YamlWriter-Objekte verwenden, um die Daten aus einer YAML-Datei zu lesen und zu schreiben.

```Java
// Ein YamlReader zum Lesen der Datei
YamlReader reader = new YamlReader(new FileReader("example.yaml"));

// Ein YamlWriter zum Schreiben von Daten in eine YAML-Datei
YamlWriter writer = new YamlWriter(new FileWriter("example.yaml"));
```

### Benutzerdefinierte Typen

YAML unterstützt die Verwendung von benutzerdefinierten Typen, um eine präzisere Verarbeitung der Daten zu ermöglichen. Wir können z.B. eine Klasse erstellen, die das `YamlSerializable`-Interface implementiert, um die Daten in der YAML-Datei auf eine bestimmte Weise zu strukturieren.

### Fehlerbehandlung

Beim Arbeiten mit YAML-Dateien ist es wichtig, auf Fehler zu achten. Wir können z.B. eine try-catch-Anweisung verwenden, um sicherzustellen, dass unsere Anwendung richtig funktioniert, auch wenn die YAML-Datei ungültige Daten enthält.

## Siehe auch

- [YAML-Spezifikation](https://yaml.org/spec/1.2/spec.html)
- [YAML-Tutorial](https://www.baeldung.com/java-yaml)
- [SnakeYaml-Dokumentation](https://bitbucket.org/asomov/snakeyaml/wiki/Documentation)