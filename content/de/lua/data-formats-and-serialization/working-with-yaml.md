---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:25:54.647298-07:00
description: "YAML, eine Abk\xFCrzung f\xFCr \"YAML Ain't Markup Language\", ist ein\
  \ f\xFCr Menschen lesbarer Daten-Serialisierungsstandard, der oft f\xFCr Konfigurationsdateien\
  \ und\u2026"
lastmod: 2024-02-19 22:05:12.966356
model: gpt-4-0125-preview
summary: "YAML, eine Abk\xFCrzung f\xFCr \"YAML Ain't Markup Language\", ist ein f\xFC\
  r Menschen lesbarer Daten-Serialisierungsstandard, der oft f\xFCr Konfigurationsdateien\
  \ und\u2026"
title: Arbeiten mit YAML
---

{{< edit_this_page >}}

## Was & Warum?

YAML, eine Abkürzung für "YAML Ain't Markup Language", ist ein für Menschen lesbarer Daten-Serialisierungsstandard, der oft für Konfigurationsdateien und den Datenaustausch zwischen Sprachen verwendet wird. Programmierer nutzen YAML aufgrund seiner Einfachheit und Lesbarkeit, was es zur bevorzugten Wahl für Einstellungen, diverse Anwendungskonfigurationen oder Inhalte macht, die von Nicht-Programmierern bearbeitet werden sollen.

## Wie geht das:

Lua hat keine integrierte Unterstützung für YAML, aber man kann mit YAML-Dateien arbeiten, indem man Drittanbieter-Bibliotheken wie `lyaml` verwendet. Diese Bibliothek ermöglicht das Kodieren und Dekodieren von YAML-Daten mit Lua. Zuerst müssen Sie `lyaml` über LuaRocks, den Paketmanager von Lua, installieren:

```bash
luarocks install lyaml
```

### YAML dekodieren:

Nehmen wir an, Sie haben den folgenden YAML-Inhalt in einer Datei namens `config.yaml`:

```yaml
database:
  host: localhost
  port: 3306
  username: user
  password: pass
```

Sie können diese YAML-Datei in eine Lua-Tabelle dekodieren mit folgendem Code:

```lua
local yaml = require('lyaml')
local file = io.open("config.yaml", "r")
local content = file:read("*all")
file:close()

local data = yaml.load(content)
for k,v in pairs(data.database) do
  print(k .. ": " .. v)
end
```

Wenn Sie dieses Skript ausführen, sollte es ausgeben:

```output
host: localhost
port: 3306
username: user
password: pass
```

### YAML kodieren:

Um Lua-Tabellen in das YAML-Format zu kodieren, verwenden Sie die Funktion `dump`, die von `lyaml` bereitgestellt wird. Angenommen, Sie möchten eine YAML-Darstellung der folgenden Lua-Tabelle erstellen:

```lua
local data = {
  website = {
    name = "Beispiel",
    owner = "Jane Doe",
    metadata = {
      creation_date = "2023-01-01",
      tags = {"blog", "personal", "lua"}
    }
  }
}

local yaml = require('lyaml')
local yaml_data = yaml.dump({data})
print(yaml_data)
```

Das ausgegebene YAML wird sein:

```yaml
- website:
    metadata:
      creation_date: '2023-01-01'
      tags: [blog, personal, lua]
    name: Beispiel
    owner: Jane Doe
```

Indem sie diesen Mustern folgen, können Lua-Programmierer YAML-Daten für eine Vielzahl von Anwendungen effektiv verwalten. Diese Operationen mit YAML sind entscheidend für die Entwicklung vielseitiger Lua-Anwendungen, die reibungslos mit anderen Teilen eines Systems oder direkt mit anderen Systemen interagieren.
