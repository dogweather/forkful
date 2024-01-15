---
title:                "Arbeiten mit YAML"
html_title:           "Fish Shell: Arbeiten mit YAML"
simple_title:         "Arbeiten mit YAML"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/fish-shell/working-with-yaml.md"
---

{{< edit_this_page >}}

## Warum

Es ist wichtig, die Grundlagen von YAML zu verstehen, da es in der heutigen Entwicklerwelt weit verbreitet ist und sowohl für individuelle Projekte als auch für den Einsatz in Unternehmen verwendet wird.

## Wie es funktioniert

Um YAML mit Fish Shell zu verwenden, müssen wir das entsprechende Plugin installieren. Dazu können wir einfach folgenden Befehl ausführen:

```Fish Shell
omf install yaml
```

Nachdem wir das Plugin installiert haben, können wir YAML-Dateien in der Fish Shell verwenden, indem wir sie in unserer Konfigurationsdatei aufrufen. Hier ist ein Beispiel für die Verwendung von YAML, um Umgebungsvariablen festzulegen:

```Fish Shell
set -gx PYTHONPATH (cat config.yml | yaml get py_path)
```

Wir können auch YAML-Dateien direkt innerhalb der Fish Shell erstellen und bearbeiten. Hier ist ein Beispiel, wie wir eine neue Datei erstellen und einige Daten in sie eingeben können:

```Fish Shell
touch new_config.yml
yaml set name "John Doe" new_config.yml
yaml set age 25 new_config.yml
yaml set favorite_color "Blue" new_config.yml
```

Dies erstellt eine neue YAML-Datei namens "new_config.yml" mit den angegebenen Daten.

## Tiefergehende Einblicke

YAML verwendet eine einfache Syntax, die das Lesen und Schreiben von Daten erleichtert. Es ist auch sehr flexibel und ermöglicht es uns, komplexe Datenstrukturen zu erstellen. Innerhalb der Fish Shell können wir YAML-Dateien verwenden, um unsere Konfigurationen zu verwalten, Umgebungsvariablen festzulegen oder sogar Skripte zu schreiben.

Weitere Informationen über die Verwendung von YAML mit Fish Shell finden Sie in der offiziellen Dokumentation des Plugins oder auf der offiziellen YAML-Website.

## Siehe auch

- [Fish Shell Dokumentation](https://fishshell.com/docs/current/)
- [YAML offizielle Website](https://yaml.org/)