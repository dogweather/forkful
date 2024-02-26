---
date: 2024-01-26 04:20:34.391271-07:00
description: "Mit TOML arbeiten bedeutet, TOML-Daten (Toms Obvious, Minimal Language)\
  \ in Elixir zu parsen und zu generieren. Programmierer nutzen es, um\u2026"
lastmod: '2024-02-25T18:49:50.681756-07:00'
model: gpt-4-0125-preview
summary: "Mit TOML arbeiten bedeutet, TOML-Daten (Toms Obvious, Minimal Language)\
  \ in Elixir zu parsen und zu generieren. Programmierer nutzen es, um\u2026"
title: Arbeiten mit TOML
---

{{< edit_this_page >}}

## Was & Warum?
Mit TOML arbeiten bedeutet, TOML-Daten (Toms Obvious, Minimal Language) in Elixir zu parsen und zu generieren. Programmierer nutzen es, um Konfigurationsdateien zu verwalten, da TOML lesbar, einfach zu parsen ist und gut auf eine Hash-Datenstruktur abgebildet werden kann.

## Wie geht das:
Fügen Sie zunächst einen TOML-Parser zu Ihren Mix-Abhängigkeiten hinzu. Dieses Beispiel verwendet `toml-elixir`:

```elixir
def deps do
  [
    {:toml_elixir, "~> 2.0"}
  ]
end
```

Eine TOML-Datei lesen:

```elixir
{:ok, toml_data} = File.read("config.toml")
{:ok, parsed_data} = TomlElixir.parse(toml_data)
```

Um Elixir-Daten in TOML zu konvertieren:

```elixir
data = %{title: "TOML-Beispiel", owner: %{name: "Tom Preston-Werner"}}
toml_string = TomlElixir.encode(data)
```

Beispielausgabe:

```elixir
"title = \"TOML-Beispiel\"\n\n[owner]\nname = \"Tom Preston-Werner\"\n"
```

## Tieferer Einblick
TOML wurde von Tom Preston-Werner, dem Mitbegründer von GitHub, für die Verwendung in Konfigurationsdateien erstellt. Es ist darauf ausgelegt, einfacher als XML und prägnanter als YAML zu sein, während es Konsistenz bewahrt.

Alternativen umfassen JSON-, YAML- und INI-Dateien, die jeweils ihre Vor- und Nachteile in Bezug auf menschliche Lesbarkeit und Datenstrukturkompatibilität haben. TOML zeichnet sich darin aus, tabellarische Daten und verschachtelte Datengruppierungen klar darzustellen.

In Elixir hängt die TOML-Verarbeitung von Dekodierungs- und Kodierungsbibliotheken ab, die TOML-Strings in Elixir-Maps umwandeln und umgekehrt. Das Parsen funktioniert, indem die Syntaxregeln von TOML abgeglichen und in Elixirs Datentypen umgewandelt werden. Die Kodierung macht das Gegenteil, indem sie Elixirs Datentypen zurück in gültige TOML-Syntax abbildet.

## Siehe auch
- TOML-Sprache: https://toml.io/en/
- `toml-elixir` GitHub-Repository: https://github.com/bitwalker/toml-elixir
- Hex-Paketdetails für `toml-elixir`: https://hex.pm/packages/toml_elixir
