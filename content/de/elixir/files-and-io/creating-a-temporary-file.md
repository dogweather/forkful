---
date: 2024-01-20 17:39:59.006585-07:00
description: "Ein tempor\xE4res File ist eine kurzlebige Datei, die w\xE4hrend der\
  \ Ausf\xFChrung eines Programms erstellt wird. Programmierer nutzen tempor\xE4re\
  \ Dateien f\xFCr\u2026"
lastmod: '2024-03-13T22:44:53.555202-06:00'
model: gpt-4-1106-preview
summary: "Ein tempor\xE4res File ist eine kurzlebige Datei, die w\xE4hrend der Ausf\xFC\
  hrung eines Programms erstellt wird. Programmierer nutzen tempor\xE4re Dateien f\xFC\
  r\u2026"
title: "Erstellung einer tempor\xE4ren Datei"
---

{{< edit_this_page >}}

## Was & Warum?
Ein temporäres File ist eine kurzlebige Datei, die während der Ausführung eines Programms erstellt wird. Programmierer nutzen temporäre Dateien für kurzfristige Speicherung, z.B. zum sicheren Umgang mit großen Datenmengen oder zum Zwischenspeichern von Daten während langer Berechnungen.

## Vorgehensweise:
Elixir hat keine eingebaute Funktion für temporäre Dateien wie in einigen anderen Sprachen. Stattdessen verwendet man oft das Betriebssystem direkt. Hier ein Beispiel:

```elixir
# Elixir-Mix-Umgebung vorbereiten
mix new temp_file_demo
cd temp_file_demo

# Erlang's :os.cmd nutzen, um ein temporäres File zu erstellen
temp_file = :os.cmd('mktemp')

# In das temporäre File schreiben
:file.write(temp_file, "Hello temporary world!")

# Inhalt des temporären Files lesen
IO.puts(File.read!(temp_file))

# Temporäres File löschen
:file.delete(temp_file)
```

Lauf des Codes sollte ausgeben:

```
Hello temporary world!
```

## Deep Dive:
Historisch gesehen griffen Elixir-Programme oft auf Erlang-Funktionen zurück, um mit dem System zu interagieren – so auch für temporäre Dateien. Man könnte auch `System.cmd/3` verwenden, um ein temporäres File auf eine dem Betriebssystem entsprechende Weise zu erzeugen. Betriebssystemspezifische Unterschiede bedeuten allerdings, dass was auf Unix-Systemen funktioniert, unter Windows fehlschlagen könnte. Mit Bibliotheken wie `Tmp` können solche Kompatibilitätsfragen elegant gelöst werden. Diese nutzen oft das „resource handling“ von BEAM (der Elixir/Erlang-Maschine), um sicherzugehen, dass temporäre Dateien nach Gebrauch gelöscht werden.

## Siehe auch:
- Elixir Dokumentation: https://elixir-lang.org/docs.html
- Erlang :os Modul: http://erlang.org/doc/man/os.html
- Erlang :file Modul: http://erlang.org/doc/man/file.html
- Tmp Library auf Hex: https://hex.pm/packages/tmp
