---
title:                "Arbeiten mit JSON"
date:                  2024-01-19
html_title:           "Arduino: Arbeiten mit JSON"
simple_title:         "Arbeiten mit JSON"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/fish-shell/working-with-json.md"
---

{{< edit_this_page >}}

## Was & Warum?
JSON steht für "JavaScript Object Notation" und ist ein kompaktes Datenaustauschformat. Programmierer nutzen JSON, um Daten strukturiert zu speichern und zwischen Servern sowie Web-Anwendungen zu übermitteln.

## How to:
Fish Shell macht es einfach, mit JSON zu arbeiten, indem es Tools wie `jq` unterstützt. Hier ein paar Beispiele, wie man JSON verarbeitet:

```
# JSON-String in eine Variable speichern
set json_string '{"name": "Fisch", "typ": "Shell"}'

# Den Wert von 'name' extrahieren
echo $json_string | jq '.name'
```
Ausgabe:
```
"Fisch"
```

```
# JSON aus einer Datei lesen
set file_path 'example.json'

# 'typ' aus der Datei extrahieren
jq '.typ' $file_path
```
Ausgabe:
```
"Shell"
```

## Deep Dive
JSON entstand aus JavaScript, ist aber sprachunabhängig. Beliebte Alternativen sind XML und YAML, die ebenfalls Daten strukturieren. JSON punktet mit seiner Einfachheit und Lesbarkeit. Fish Shell selbst bietet keine eingebaute JSON-Verarbeitung, aber Tools wie `jq` füllen diese Lücke effektiv und sind einfach zu integrieren.

## See Also
- Die offizielle `jq`-Dokumentation: https://stedolan.github.io/jq/manual/
- Fish Shell Dokumentation: https://fishshell.com/docs/current/index.html
- JSON-Format Spezifikation: https://www.json.org/json-de.html
