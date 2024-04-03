---
date: 2024-01-26 04:21:19.576908-07:00
description: "TOML ist ein Konfigurationsdateiformat, das f\xFCr Menschen leicht zu\
  \ lesen und zu schreiben ist und von Maschinen einfach geparst und generiert werden\
  \ kann.\u2026"
lastmod: '2024-03-13T22:44:54.332089-06:00'
model: gpt-4-0125-preview
summary: "TOML ist ein Konfigurationsdateiformat, das f\xFCr Menschen leicht zu lesen\
  \ und zu schreiben ist und von Maschinen einfach geparst und generiert werden kann."
title: Arbeiten mit TOML
weight: 39
---

## Wie geht das:
Um TOML in Fish zu lesen und zu manipulieren, kann man ein Werkzeug wie `yj` verwenden, das TOML in JSON konvertieren kann. So geht's:

```fish
# Installiere yj über Fisher
fisher install jorgebucaran/yj

# Konvertiere TOML zu JSON
echo 'title = "TOML Beispiel"' | yj -tj

# Beispiel Ausgabe
{"title":"TOML Beispiel"}
```

Um TOML zu schreiben, kehrt man den Prozess um:

```fish
# Konvertiere JSON zu TOML
echo '{"title":"JSON Beispiel"}' | yj -jt

# Beispiel Ausgabe
title = "JSON Beispiel"
```

Für umfangreichere Aufgaben sollten Sie ein dediziertes TOML-CLI-Werkzeug wie `toml-cli` in Betracht ziehen.

```fish
# Installiere toml-cli
pip install toml-cli

# Setze einen Wert in einer TOML-Datei
toml set pyproject.toml tool.poetry.version "1.1.4"

# Hole einen Wert aus einer TOML-Datei
set version (toml get pyproject.toml tool.poetry.version)
echo $version
```

## Tiefergehend
TOML (Tom's Obvious, Minimal Language), 2013 von Tom Preston-Werner eingeführt, ähnelt INI, verfügt aber über eine definierte Spezifikation und Datenhierarchie. Die Hauptalternativen sind JSON und YAML, aber diese haben ihre Vor- und Nachteile: JSON ist nicht so benutzerfreundlich, während YAML komplexer ist. Das Design von TOML blüht in Szenarien auf, in denen Konfigurationsdateien oft von Hand gepflegt werden, und bietet eine Balance zwischen Einfachheit und Ausdruckskraft. Was die Implementierung betrifft, so sind TOML-Parser für die meisten Programmiersprachen verfügbar, einschließlich TomlBombadil für Fish, das sich direkt in Ihre Skripte integrieren lässt.

## Siehe auch
- Offizielle TOML-Spezifikation: https://toml.io
- `yj`, ein Werkzeug zur Konvertierung zwischen TOML, JSON, YAML und XML: https://github.com/jorgebucaran/yj
- `toml-cli`, ein Kommandozeilen-Utility für TOML: https://github.com/sdispater/toml-cli
