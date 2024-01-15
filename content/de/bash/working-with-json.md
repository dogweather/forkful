---
title:                "Arbeiten mit JSON"
html_title:           "Bash: Arbeiten mit JSON"
simple_title:         "Arbeiten mit JSON"
programming_language: "Bash"
category:             "Bash"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/bash/working-with-json.md"
---

{{< edit_this_page >}}

## Warum

Als aktuellste Version der Bash-Skriptsprache ist es wichtig, sich mit dem Arbeiten mit JSON vertraut zu machen, da dies immer häufiger in der Programmierung eingesetzt wird. Mit Bash können JSON-Daten schnell und effizient verarbeitet werden, was die Entwicklung von Skripten erleichtert.

## Wie funktioniert es?

Um mit JSON in Bash zu arbeiten, müssen wir zunächst sicherstellen, dass das Tool "jq" installiert ist. Mit diesem Tool können wir JSON-Daten analysieren, abfragen und manipulieren. Hier ist ein Beispiel, wie man einen bestimmten Wert aus einer JSON-Datei abruft:

```Bash
#!/bin/bash

# Laden der jq Bibliothek
. /usr/local/bin/jq

# Definieren der JSON-Datei
json='{"name": "Max Mustermann", "age": 30, "hobbies": ["tennis", "gaming", "cooking"]}'

# Abruf des Alters-Werts aus der JSON-Datei
jq '.age' <<< "$json"
```

Die Ausgabe dieses Skripts wäre: `30`, da wir den Alterswert aus der JSON-Datei abgerufen haben.

## Tiefergehende Informationen

Mit dem Tool "jq" gibt es viele Möglichkeiten, JSON-Daten zu verarbeiten. Hier sind einige weitere Beispiele:

- `jq '.hobbies[0]' <<< "$json"` würde den ersten Punkt der Hobbies-Liste ausgeben, also `tennis`.
- `jq '.hobbies[]' <<< "$json"` würde alle Hobbies ausgeben, also `tennis`, `gaming` und `cooking`.
- `jq '.hobbies[1:]' <<< "$json"` würde alle Hobbies ab dem zweiten Punkt, also `gaming` und `cooking`, ausgeben.

Für eine vollständige Dokumentation von "jq" können Sie die offizielle Webseite besuchen: https://stedolan.github.io/jq/.

## Siehe auch

- Offizielle Website von "jq": https://stedolan.github.io/jq/.
- Ein Tutorial zur Verwendung von "jq": https://www.baeldung.com/linux/jq-command-json.
- Ein interaktives Spiel zum Lernen von "jq": https://try.jqplay.org/.