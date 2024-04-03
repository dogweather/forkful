---
date: 2024-01-26 04:25:34.432597-07:00
description: "TOML, eine Abk\xFCrzung f\xFCr Toms Offensichtliche, Minimale Sprache,\
  \ ist ein Daten-Serialisierungsformat \xE4hnlich wie JSON oder YAML, zielt jedoch\
  \ auf\u2026"
lastmod: '2024-03-13T22:44:53.401921-06:00'
model: gpt-4-0125-preview
summary: "TOML, eine Abk\xFCrzung f\xFCr Toms Offensichtliche, Minimale Sprache, ist\
  \ ein Daten-Serialisierungsformat \xE4hnlich wie JSON oder YAML, zielt jedoch auf\
  \ Einfachheit und Lesbarkeit ab."
title: Arbeiten mit TOML
weight: 39
---

## Wie:
Bevor Sie beginnen, installieren Sie das `toml`-Paket mit `pip install toml`. Lassen Sie uns eine TOML-Datei parsen:

```python
import toml

# Beispiel TOML-Inhalt als Zeichenkette
toml_zeichenkette = """
[owner]
name = "Tom Preston-Werner"
dob = 1979-05-27T07:32:00Z # Erstklassige Daten

[database]
server = "192.168.1.1"
ports = [ 8001, 8001, 8002 ]
"""

# Die TOML-Zeichenkette parsen
geparstes_toml = toml.loads(toml_zeichenkette)

# Auf Daten zugreifen
print(geparstes_toml['owner']['name'])  # Ausgabe: Tom Preston-Werner
print(geparstes_toml['database']['ports'])  # Ausgabe: [8001, 8001, 8002]
```

## Tiefere Betrachtung
TOML wurde von Tom Preston-Werner, einem der Gründer von GitHub, als eine benutzerfreundlichere Konfigurationsdateiform geschaffen. Es ist darauf ausgelegt, eindeutig auf eine Hash-Tabelle abgebildet zu werden, und leicht von Maschinen zu parsen.

Im Vergleich zu JSON ist TOML lesbarer für Konfigurationsdateien und unterstützt Kommentare. YAML, eine andere Alternative, kann kompakter sein, aber seine Abhängigkeit von Einrückungen und subtile Probleme, wie das Verbot von Tabulatoren, können Leute stolpern lassen.

Was die Implementierungsdetails betrifft, so sind TOML-Werte getypt, was Zeichenketten, ganze Zahlen, Fließkommazahlen, Booleans, Datumsangaben, Arrays und Tabellen umfasst. Alles ist groß- und kleinschreibungsempfindlich. Außerdem unterstützt TOML mehrzeilige Zeichenketten und erlaubt in der neuesten Version sogar heterogen typisierte Arrays.

Python verwendet die `toml`-Bibliothek, die in Bezug auf die API den JSON- und YAML-Bibliotheken ähnelt. Sie haben `toml.load` und `toml.loads` zum Lesen von TOML aus einer Datei oder einem String bzw. `toml.dump` und `toml.dumps` zum Schreiben.

## Siehe auch
- Das offizielle TOML GitHub-Verzeichnis für Spezifikationen: [github.com/toml-lang/toml](https://github.com/toml-lang/toml)
- Die Dokumentation der `toml`-Bibliothek in Python: [pypi.org/project/toml/](https://pypi.org/project/toml/)
- Praxisbeispiele für TOML: Konfigurationsdateien für Rusts Paketmanager `cargo` oder das Python-Verpackungswerkzeug `poetry`.
