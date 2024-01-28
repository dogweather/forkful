---
title:                "Arbeiten mit TOML"
date:                  2024-01-26T04:22:01.918589-07:00
model:                 gpt-4-0125-preview
simple_title:         "Arbeiten mit TOML"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/working-with-toml.md"
---

{{< edit_this_page >}}

## Was & Warum?
Mit TOML zu arbeiten, bedeutet TOML-Dateien (Tom's Offensichtliche, Minimale Sprache) mit Code zu parsen und zu generieren. Programmierer nutzen TOML für leicht lesbare Konfigurationsdateien und Datenserialisierung, dank seiner klaren Semantik und Kompatibilität mit konventionellen Datentypen.

## Wie:
Gleam hat keine integrierte TOML-Unterstützung, daher benötigst du eine externe Bibliothek. Zum Beispiel:

```gleam
// Angenommen, du hast eine TOML-Parsing-Bibliothek:
import toml/{Parser, Encoder}

// TOML-Inhalt parsen
let toml_content = """
[owner]
name = "Tom Preston-Werner"
dob = 1979-05-27T07:32:00Z
"""

let parsed = Parser.parse(toml_content)

// Die geparsten Daten verwenden
match parsed {
  Ok(data) -> "Daten erfolgreich geparst!"
  Error(_) -> "Fehler beim Parsen der Daten."
}

// TOML-Inhalt aus einer Gleam-Datenstruktur generieren
let data = #{
  "owner": #{
    "name": "Tom Preston-Werner",
    "dob": "1979-05-27T07:32:00Z"
  }
}

let toml_string = Encoder.encode(data)
```

Beispielausgabe:

```
Daten erfolgreich geparst!
```

## Tiefergehend
TOML wurde 2013 von Tom Preston-Werner veröffentlicht. Sein Ziel: Lesbarer und geradliniger als XML und weniger komplex als YAML für Dateikonfigurationen zu sein. Trotz der Einfachheit ist es robust für strukturierte Daten und bietet eine explizite und leicht verständliche Syntax. Alternativen umfassen JSON, YAML und INI, aber die minimalistische und klare Syntax von TOML gewinnt oft für Konfigurationsdateien. TOML in Gleam zu implementieren, bedeutet vor allem zwei Dinge: TOML in native Datenstrukturen zu parsen und native Datenstrukturen in TOML zu serialisieren. Die meisten TOML-Bibliotheken für Erlang oder Elixir können in Gleam verwendet werden, aufgrund seiner Interoperabilität mit BEAM-Sprachen, was eine nahtlose Integration in Gleam-Projekte gewährleistet.

## Siehe auch
- TOML-Sprachspezifikationen: [https://toml.io/en/](https://toml.io/en/)
- Ein Erlang TOML-Parser: [https://hex.pm/packages/toml](https://hex.pm/packages/toml)
- TOML auf GitHub: [https://github.com/toml-lang/toml](https://github.com/toml-lang/toml)
