---
title:                "Arbeiten mit JSON"
date:                  2024-01-19
html_title:           "Arduino: Arbeiten mit JSON"
simple_title:         "Arbeiten mit JSON"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/bash/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why?
JSON steht für JavaScript Object Notation. Programmeure nutzen es, um Daten zwischen Servern und Webapplikationen auszutauschen. Es ist leicht lesbar für Menschen und einfach zu parsen für Maschinen.

## How to:
Du kannst `jq` nutzen, ein Befehlszeilentool zum Verarbeiten von JSON. Hier einige Beispiele:

```Bash
# JSON-Datei einlesen und schön ausgeben:
cat datei.json | jq '.'

# Wert eines Schlüssels ausgeben:
echo '{"name": "Tom", "age": 31}' | jq '.name'

# Filter für mehrere Werte:
echo '{"name": "Tom", "age": 31}' | jq '. | {name: .name, age: .age}'
```

Ausgaben:

```Bash
{
  "name": "Tom",
  "age": 31
}
"Tom"
{
  "name": "Tom",
  "age": 31
}
```

## Deep Dive:
Bereits seit den frühen 2000er Jahren ist JSON als Datenformat bekannt. Es ist das Hauptalternativformat zu XML, da es leichter und schneller zu parsen ist. `jq` kann mit Bash-Scripts kombiniert werden, um komplexe Aufgaben beim Umgang mit JSON zu bewältigen.

## See Also:
- JSON-Spezifikation: https://www.json.org/json-de.html
- `jq` Tutorial: https://stedolan.github.io/jq/tutorial/
- Bash-Scripting-Lernressourcen: https://www.shellscript.sh/
