---
title:                "Arbeiten mit YAML"
html_title:           "Bash: Arbeiten mit YAML"
simple_title:         "Arbeiten mit YAML"
programming_language: "Bash"
category:             "Bash"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/bash/working-with-yaml.md"
---

{{< edit_this_page >}}

## Warum

Wenn Sie schon einmal mit Konfigurationsdateien gearbeitet haben, haben Sie wahrscheinlich von YAML gehört. YAML steht für "YAML Ain't Markup Language" und ist eine einfache, lesbare Auszeichnungssprache, die für Konfigurationsdateien verwendet wird. Es ist nützlich für Entwickler, Systemadministratoren und jeder, der mit strukturierten Daten arbeitet.

## Wie man mit YAML in Bash arbeitet

Um mit YAML in Bash zu arbeiten, benötigen Sie ein paar grundlegende Befehle und eine YAML-Parser-Bibliothek namens "YQ". Hier sind einige Beispiele, wie Sie YAML in Bash nutzen können:

```bash
# Installieren von yq
sudo apt install yq

# Ausgabe des Inhalts einer YAML-Datei
yq read config.yml

# Ausgabe von bestimmten Werten aus der YAML-Datei
yq read config.yml 'section.subsection.value'

# Aktualisieren einer YAML-Datei
yq write config.yml 'section.subsection.value' 'new value'
```

Beachten Sie, dass yq das Ergebnis in JSON formatiert, das für Bash leichter zu verarbeiten ist. Um den Output als YAML zu erhalten, können Sie den Befehl `yq read --prettyPrint config.yml` verwenden.

Wenn Sie mehr über die yq-Befehle und deren Verwendung erfahren möchten, können Sie die offizielle Dokumentation [hier](https://mikefarah.gitbook.io/yq/) nachlesen.

## Tieferes Eintauchen

Es gibt viele Möglichkeiten, wie Sie YAML in Bash nutzen können. Sie können z.B. Integer, Float, Strings, Listen und sogar komplexe Datenstrukturen wie Dictionary und Nested-Attribute in YAML definieren. Wenn Sie sich in die Tiefe begeben möchten, können Sie lernen, wie Sie diese Datentypen in Bash verwenden und verarbeiten können. Sie können auch verschiedene Tools und Bibliotheken erkunden, die speziell für die Arbeit mit YAML in Bash entwickelt wurden.

## Siehe auch

- [YAML offizielle Website](https://yaml.org/)
- [YQ Dokumentation](https://mikefarah.gitbook.io/yq/)
- [Bash Dokumentation](https://www.gnu.org/software/bash/manual/)
- [YAML Syntax Überblick](https://learnxinyminutes.com/docs/yaml/)