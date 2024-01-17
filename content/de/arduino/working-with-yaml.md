---
title:                "Arbeiten mit yaml"
html_title:           "Arduino: Arbeiten mit yaml"
simple_title:         "Arbeiten mit yaml"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/working-with-yaml.md"
---

{{< edit_this_page >}}

## Was ist YAML und warum benutzen Programmierer es?

YAML ist eine vereinfachte, benutzerfreundliche Möglichkeit, Daten in einem strukturierten Format zu speichern. Es ist besonders nützlich für Entwickler, da es ihnen ermöglicht, verschiedene Arten von Daten zu organisieren und zu verwalten. Es ist auch leicht lesbar und leicht verständlich, was es zu einem beliebten Wahl unter Programmierern macht.

## Wie funktioniert es?

Um mit YAML in Arduino zu arbeiten, müssen wir zunächst die YAML-Header-Datei einbeziehen. Dies geschieht durch Hinzufügen der folgenden Zeile zu Ihrem Code:

```Arduino
#include <YAML.h>
```

Als nächstes müssen wir eine YAML-Variablendeklaration erstellen, um unsere Daten in einem strukturierten Format zu speichern. Dies kann wie folgt aussehen:

```Arduino
YAML::Node myData;
```

Um nun Daten zu speichern, können wir auf die Memberfunktion von YAML zugreifen und einen Schlüsselwert hinzufügen. Zum Beispiel:

```Arduino
myData["Name"] = "Max Mustermann";
```

Um auf die gespeicherten Daten zuzugreifen, können wir einfach den Schlüsselwert abrufen. Zum Beispiel:

```Arduino
Serial.println(myData["Name"]);
```

## Tiefere Einblicke

YAML wurde ursprünglich als einfache Auszeichnungs- und Datenformatierungssprache für Programmierer entwickelt. Es ist jedoch in den letzten Jahren immer beliebter geworden und wird mittlerweile auch in anderen Anwendungen wie Konfigurationsdateien und Datenbanken verwendet.

Alternativen zu YAML sind JSON und XML, die ebenfalls strukturierte Daten formen. Der Vorteil von YAML gegenüber diesen Alternativen ist jedoch seine Lesbarkeit und Benutzerfreundlichkeit.

Die Implementierung von YAML in Arduino ist relativ einfach, da es eine offizielle Header-Datei gibt, die in den Arduino Sketch importiert werden kann. Es gibt auch viele Ressourcen und Tutorials online, um Ihnen bei der Arbeit mit YAML in Arduino zu helfen.

## Weitere Informationen

Hier sind einige nützliche Links, um mehr über YAML zu erfahren und wie es in Arduino implementiert wird:

- Offizielle YAML-Website: https://yaml.org/
- Arduino YAML-Bibliothek: https://github.com/arduino-libraries/YAML
- YAML-Spezifikation: https://yaml.org/spec/
- Tutorial zur Verwendung von YAML mit Arduino: https://learn.sparkfun.com/tutorials/working-with-yaml/all