---
title:                "Arbeiten mit YAML"
html_title:           "Elm: Arbeiten mit YAML"
simple_title:         "Arbeiten mit YAML"
programming_language: "Elm"
category:             "Elm"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elm/working-with-yaml.md"
---

{{< edit_this_page >}}

## Was & Warum?
Arbeiten mit YAML ist eine gängige Aufgabe für Programmierer. YAML (Yet Another Markup Language) ist eine einfache, menschenlesbare Datenstruktur, die häufig in der Softwareentwicklung verwendet wird. Es ist vorteilhaft, da es leicht zu verarbeiten und zu verstehen ist, was es zu einer beliebten Wahl unter Entwicklern macht.

## So geht's:
Ein einfaches Beispiel, um eine YAML-Datei in Elm zu lesen, wäre:

```elm
import Http
import Json.Decode as Decode
import Yaml.Decode as Yaml

type alias Person =
  { name : String
  , age : Int 
  }

--- Laden der YAML-Datei "person.yml"
loadFile =
  let
    url = "person.yml"
    request = Http.get url Decode.string
  in
    Decode.andThen
      ( Yaml.decodeString Yaml.value
          |> Decode.map (\person -> ({ name = Yaml.field "Name" Yaml.string person
                                     , age = Yaml.field "Alter" Yaml.int person }) )
  )
    request

--- Verwendung der Funktion
getPerson =
  case loadFile of
    Ok person -> person
    Err err -> Debug.log "Fehler beim Laden der Datei:" err

```

Die Inhalte der YAML-Datei "person.yml" könnten wie folgt aussehen:

```yaml
Name: Max Mustermann
Alter: 30
```

Die Funktion `getPerson` würde dann ein `Ok` Resultat mit dem entsprechenden Person-Objekt zurückgeben.

## Tiefere Einblicke:

### Historischer Kontext:
YAML wurde erstmals im Jahr 2001 von Clark Evans entwickelt und sollte als ein benutzerfreundliches Alternativformat zu XML dienen. Es ist eine vereinfachte Version von XML und wird häufig als Datenstruktur für Konfigurationsdateien, Datenübertragungen zwischen Systemen oder für die Speicherung von Anwendungsdaten verwendet.

### Alternativen:
Obwohl YAML eine beliebte Wahl unter Programmierern ist, gibt es einige alternative Formate wie beispielsweise JSON oder XML, die ebenfalls für ähnliche Zwecke verwendet werden können. Die Wahl des geeigneten Formats hängt oft von den Bedürfnissen und Präferenzen des Teams oder Projekts ab.

### Implementierungsdetails:
In Elm wird das `Yaml.Decode` Paket verwendet, um eine YAML-Datei zu verarbeiten. Dabei werden die Daten in eine Json-Wertstruktur übersetzt, die dann wiederum in das Elm-Datenmodell konvertiert werden kann. Dieser Prozess ermöglicht eine einfache Verwendung von YAML-Daten in Elm.

## Siehe auch:
- [Offizielle Dokumentation von Elm](https://elm-lang.org/)
- [YAML-Spezifikation](https://yaml.org/spec/)