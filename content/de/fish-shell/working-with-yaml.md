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

Fish Shell Programmier-Artikel: Einführung in YAML

## Was ist YAML und warum benutzen Programmierer es?

YAML steht für "YAML Ain't a Markup Language" und ist eine einfache, menschenlesbare Formatierungssprache, die hauptsächlich für das Speichern und Übertragen von Daten verwendet wird. Programmierer verwenden YAML, um komplexe Datenstrukturen wie Konfigurationsdateien, Objektmodelle und andere strukturierte Daten zu organisieren und zu speichern. Es ist auch sehr nützlich für die Zusammenarbeit in gängigen Projekten, da es leicht zu lesen und zu bearbeiten ist.

## Wie verwendet man YAML in der Fish Shell?

Fish Shell hat eine eingebaute Funktion, um mit YAML-Dateien zu arbeiten. Mit dem Befehl ```YAMLop``` können wir YAML-Dateien öffnen und die darin enthaltenen Daten extrahieren. Hier ist ein einfaches Beispiel:

```
YAMLop config.yml
```

Dies wird die Daten in der YAML-Datei "config.yml" ausgeben. Wenn wir beispielsweise die folgende YAML-Datei haben:

```
titel: Mein tolles Projekt
beschreibung: Dies ist eine Beschreibung meines Projekts
autor:
  name: Max Mustermann
  email: max.mustermann@beispiel.com
```

Der Befehl ```YAMLop config.yml``` würde Folgendes ausgeben:

```
titel: Mein tolles Projekt
beschreibung: Dies ist eine Beschreibung meines Projekts
autor:
  name: Max Mustermann
  email: max.mustermann@beispiel.com
```

## Tiefer Einblick

YAML wurde ursprünglich von Clark Evans als eine einfache Alternative zu XML entwickelt und im Jahr 2001 erstmals veröffentlicht. Es ist in der Programmierwelt sehr beliebt geworden, insbesondere für Projekte, die eine leicht lesbare und bearbeitbare Konfigurationsdatei benötigen.

Es gibt auch andere Möglichkeiten, mit YAML in der Fish Shell zu arbeiten. Zum Beispiel können wir die ```YAMLencode```-Funktion verwenden, um Daten in YAML-Format zu codieren oder die ```YAMLto```-Funktion, um Daten in anderen Formaten wie JSON oder CSV zu konvertieren.

## Weitere Informationen

Für weitere Informationen zum Arbeiten mit YAML in der Fish Shell können wir die offizielle Dokumentation besuchen: https://fishshell.com/docs/current/cmds/YAMLop.html

Um mehr über die YAML-Syntax und ihre Verwendung in anderen Programmiersprachen zu erfahren, kann diese Ressource hilfreich sein: https://yaml.org/

Wir hoffen, dass dieser Artikel dir dabei geholfen hat, einen Einblick in das Arbeiten mit YAML in der Fish Shell zu bekommen. Viel Spaß beim Programmieren!