---
date: 2024-01-20 17:45:17.466899-07:00
description: "So geht's: Teilstrings in Elixir zu extrahieren, ist dank des `String`-Moduls\
  \ ein Kinderspiel. Fr\xFCher, in Sprachen wie C, musste man Zeichen f\xFCr Zeichen\u2026"
lastmod: '2024-04-05T21:53:55.408695-06:00'
model: gpt-4-1106-preview
summary: Teilstrings in Elixir zu extrahieren, ist dank des `String`-Moduls ein Kinderspiel.
title: Teilstrings extrahieren
weight: 6
---

## So geht's:
```elixir
string = "Hallo, Welt!"

# Extrahiere einen Teilstring mit einer Startposition und einer Länge
teil = String.slice(string, 7, 5)
IO.puts(teil) # Ausgabe: "Welt!"

# Benutze einen Bereich, um den Teilstring zu bekommen
teil_bereich = String.slice(string, 7..11)
IO.puts(teil_bereich) # Ausgabe: "Welt!"
```

## Tiefgang
Teilstrings in Elixir zu extrahieren, ist dank des `String`-Moduls ein Kinderspiel. Früher, in Sprachen wie C, musste man Zeichen für Zeichen verarbeiten – langsam und fehleranfällig. Elixir hingegen nutzt die binäre Musterabgleichsfunktion von Erlang, was den Vorgang schnell und zuverlässig macht. Du hast Alternativen: `binary_part/3` für Binärdaten oder sogar Regex, wenn's komplizierter wird. Unter der Haube konvertiert Elixir Strings in Binärdaten und arbeitet mit Bytes, nicht mit Zeichen, was mit UTF-8 ein bisschen tricky sein kann.

## Siehe Auch
- Elixir-Dokumentation zu `String.slice/3`: https://hexdocs.pm/elixir/String.html#slice/3
- Erlang's Funktionen für Binärdaten: http://erlang.org/doc/man/binary.html
- Ruby's `String#slice` Methode zum Vergleich: https://ruby-doc.org/core/String.html#method-i-slice
