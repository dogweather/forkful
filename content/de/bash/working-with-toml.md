---
title:                "Arbeiten mit TOML"
aliases:
- de/bash/working-with-toml.md
date:                  2024-01-26T04:18:44.284314-07:00
model:                 gpt-4-0125-preview
simple_title:         "Arbeiten mit TOML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/bash/working-with-toml.md"
---

{{< edit_this_page >}}

## Was & Warum?
TOML, kurz für Toms Offensichtliche, Minimale Sprache, ist ein Format zur Serialisierung von Daten. Programmierer schätzen es wegen seiner Einfachheit und Lesbarkeit; es ist ideal für Konfigurationsdateien, ähnlich wie YAML, aber weniger umständlich als JSON für den Menschen.

## Wie geht das:
Zuerst `toml-cli` installieren, um mit TOML in Bash zu experimentieren. Praktisch für das Lesen oder Bearbeiten von TOML-Dateien spontan.

```Bash
# toml-cli installieren, unser kleiner Helfer für TOML-Aufgaben
pip install toml-cli

# Stellen Sie sich vor, Sie haben eine TOML-Datei, 'config.toml'
echo -e 'title = "TOML Demo"\n\n[owner]\nname = "Tom"\ndob = 1979-05-27T07:32:00Z' > config.toml

# Einen Wert lesen
toml get config.toml owner.name
# Ausgabe: Tom

# Einen Wert setzen
toml set config.toml 'owner.dob' '2000-01-01T00:00:00Z'
# Profi-Tipp: Verwenden Sie Anführungszeichen für Schlüssel mit Punkten oder ausgefallenen Zeichen!
```

## Tiefer Eintauchen
Entstanden aus der Abneigung gegenüber den Hürden von JSON für Menschen, wurde TOML etwa 2013 ins Leben gerufen. Tom Preston-Werner, Mitbegründer von GitHub, wollte etwas extrem Lesbares. YAML und INI waren Alternativen, aber TOML ist wie das Beste aus beiden.

Shebang, Sie haben verschachtelte Daten und Arrays, minus YAMLs Fußangeln und JSONs geschweiften Klammern. TOML ist jetzt ein Go-to für Konfiguration in Rusts Cargo, was für seinen Aufstieg in der Entwicklerwelt spricht. Es wird durch eine Spezifikation angetrieben, die alles straff und gut definiert hält. Sie werden Parser in fast jeder Sprache ergattern, was es weitgehend adoptierbar macht.

## Siehe auch
- Offizielles TOML GitHub-Repo: https://github.com/toml-lang/toml
- toml-cli auf PyPI: https://pypi.org/project/toml-cli/
- Vergleich von Daten-Serialisierungsformaten: https://de.wikipedia.org/wiki/Vergleich_von_Daten-Serialisierungsformaten
