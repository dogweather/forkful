---
title:                "Arbeiten mit TOML"
date:                  2024-01-26T04:25:34.432597-07:00
model:                 gpt-4-0125-preview
simple_title:         "Arbeiten mit TOML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/python/working-with-toml.md"
---

{{< edit_this_page >}}

## Was & Warum?
TOML, eine Abkürzung für Toms Offensichtliche, Minimale Sprache, ist ein Daten-Serialisierungsformat ähnlich wie JSON oder YAML, zielt jedoch auf Einfachheit und Lesbarkeit ab. Programmierer verwenden TOML für Konfigurationsdateien, weil es einfach zu schreiben und zu verstehen ist, und es ordnet sich sauber in Datenstrukturen in Programmiersprachen wie Python ein.

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
