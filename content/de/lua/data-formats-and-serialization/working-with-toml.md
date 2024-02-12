---
title:                "Arbeiten mit TOML"
aliases:
- /de/lua/working-with-toml/
date:                  2024-01-26T04:24:02.645470-07:00
model:                 gpt-4-0125-preview
simple_title:         "Arbeiten mit TOML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/lua/working-with-toml.md"
---

{{< edit_this_page >}}

## Was & Warum?
Die Arbeit mit TOML umfasst das Parsen und Generieren von TOML-Daten (Tom's Offensichtliche, Minimale Sprache) mit Lua. Programmierer nutzen TOML für Konfigurationsdateien aufgrund seiner Lesbarkeit und einfachen Syntax, die sich leicht in eine Datenstruktur übersetzen lässt.

## Wie geht das:
Stellen Sie zunächst sicher, dass Ihre Lua-Umgebung einen TOML-Parser hat. Wir verwenden für dieses Beispiel `lua-toml`.

```Lua
local toml = require("toml")

-- TOML-String parsen
local toml_data = [[
title = "TOML Beispiel"

[owner]
name = "Tom Preston-Werner"
dob = 1979-05-27T07:32:00Z
]]

local data = toml.parse(toml_data)
print(data.title) -- "TOML Beispiel"

-- TOML-String generieren
local table_data = {
  title = "TOML Beispiel",
  owner = {
    name = "Tom Preston-Werner",
    dob = os.time({year=1979, month=5, day=27, hour=7, min=32})
  }
}

local toml_string = toml.encode(table_data)
print(toml_string)
```

Beispielausgabe:
```
TOML Beispiel
```

## Tiefergehende Betrachtung
TOML wurde 2013 von Tom Preston-Werner als Alternative zu anderen Datenserialisierungssprachen wie XML und YAML geschaffen und bietet ein geradlinigeres Format zur Repräsentation von Konfigurationsdaten. Während JSON allgegenwärtig ist, kann seine Syntax für Konfigurationsdateien umständlich sein. TOML glänzt mit einer klareren Syntax für Menschen, ähnlich .ini-Dateien, aber mit Verschachtelungsfähigkeiten und Datentypen.

Alternativen zu TOML umfassen JSON, YAML und XML. TOML ist jedoch speziell für Konfigurationen ausgelegt und ist wohl einfacher als YAML, lesbarer als JSON für Konfigurationszwecke und weniger umfangreich als XML.

Die Implementierung der TOML-Behandlung in Lua erfordert im Allgemeinen eine Drittanbieterbibliothek. Leistung und Funktionen können variieren, von einfachem Parsen bis zur vollständigen Serialisierungsunterstützung. Wenn Sie mit großen Konfigurationsdateien oder häufigen Lese-/Schreiboperationen arbeiten, sollten Sie die Leistung der Bibliothek und die Übereinstimmung mit der neuesten TOML-Version berücksichtigen.

## Siehe auch
- TOML-Spezifikation: https://toml.io/en/
- `lua-toml` Bibliothek: https://github.com/jonstoler/lua-toml
- Vergleich von Datenserialisierungsformaten: https://de.wikipedia.org/wiki/Vergleich_von_Datenserialisierungsformaten
