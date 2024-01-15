---
title:                "Arbeiten mit JSON"
html_title:           "Fish Shell: Arbeiten mit JSON"
simple_title:         "Arbeiten mit JSON"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/fish-shell/working-with-json.md"
---

{{< edit_this_page >}}

## Warum

Warum sollte man sich überhaupt mit JSON beschäftigen? Nun, JSON ist eines der populärsten Datenaustauschformate, das von vielen Programmiersprachen und APIs unterstützt wird. Das macht es zu einem unverzichtbaren Werkzeug für Entwickler, die Daten empfangen, verarbeiten und senden möchten. Außerdem ist es leicht zu lesen und zu verstehen, was es ideal für die Kommunikation zwischen Programmen macht.

## Wie

Um mit JSON in der Fish Shell zu arbeiten, können wir die integrierten Funktionen `from_json` und `to_json` verwenden.

Um einen JSON-String in ein Array oder Objekt umzuwandeln, verwenden wir `from_json` mit dem Pipe-Operator:

```Fish Shell
set json_string '{"name": "Max Mustermann", "age": 25}'
set user_info (echo $json_string | from_json)
echo $user_info['name']   # Ausgabe: Max Mustermann
echo $user_info['age']    # Ausgabe: 25
```

Um ein Array oder Objekt in einen JSON-String zu konvertieren, verwenden wir `to_json`:

```Fish Shell
set colors (list blue red green)
set json_colors (echo $colors | to_json)
echo $json_colors   # Ausgabe: ["blue", "red", "green"]
```

## Deep Dive

Neben `from_json` und `to_json` bietet die Fish Shell auch die Funktion `json_escape`, die verwendet werden kann, um sicherzustellen, dass alle Sonderzeichen in einem JSON-String richtig escaped werden.

```Fish Shell
set special_char "My name is John \"Doe\"."
set json_string (echo $special_char | json_escape)
echo $json_string   # Ausgabe: My name is John \"Doe\".
```

Außerdem können mit der Funktion `json_parse` auch komplexe JSON-Objekte geparst werden, indem Felder durch Pfade angegeben werden.

```Fish Shell
set json_string '{"name": "Max Mustermann", "address": {"street": "Musterstraße", "city": "Musterstadt"}}'
set street_path 'address.street'
set user_street (echo $json_string | json_parse $street_path)
echo $user_street   # Ausgabe: Musterstraße
```

## Siehe auch

- [Fish Shell Dokumentation zu JSON](https://fishshell.com/docs/current/cmds/from_json.html)
- [JSON Tutorial auf Codecademy (auf Deutsch)](https://www.codecademy.com/de/tracks/javascript)
- [JSON Validator zum Überprüfen von JSON-Strukturen](https://jsonlint.com/)