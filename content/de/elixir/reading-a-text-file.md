---
title:                "Eine Textdatei lesen"
html_title:           "Bash: Eine Textdatei lesen"
simple_title:         "Eine Textdatei lesen"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Lesen einer Textdatei bezieht sich auf den Prozess, durch den ein Programm Daten aus einer Datei extrahiert. Programmierer machen das oft, um Daten für die weitere Verarbeitung zu erfassen oder Anwendungen mit persistierenden Daten zu versorgen.

## So geht's:
Im Folgenden finden Sie ein Beispiel, wie Sie mit Elixir eine Textdatei lesen können:
```elixir
File.read("meine_datei.txt")
```
Diese Funktion gibt ein Tupel zurück. Wenn der Vorgang erfolgreich ist, erhalten Sie `{:ok, data}`. Andernfalls, wenn ein Fehler auftritt, erhalten Sie `{:error, reason}`. Hier ist ein Beispiel für den erwarteten Ausgabe:
```elixir
{:ok, "Der Dateiinhalt"}
# oder
{:error, :enoent}
```

## Tiefere Einblicke
Datei-Lesevorgänge gehören zu den grundlegendsten Operationen in der Informatik. Seit den Anfängen der Computerprogrammierung haben verschiedene Sprachen unterschiedliche Methoden für diesen Vorgang bereitgestellt.

In Elixir ist `File.read/1` die einfachste Möglichkeit, eine Datei zu lesen. Es gibt jedoch auch Alternativen wie `File.stream/3`, das einen Stream zurückgibt und für große Dateien nützlich sein kann, die nicht in den Arbeitsspeicher passen.

Die Implementierung der Datei-Lebefunktionen in Elixir stützt sich auf die Funktionen des Erlang-Standardmoduls `:file` . Diese Funktionen verfolgen ein genaues Konzept und sind von den Rechten des Betriebssystems abhängig, das auf die Datei zugreifen will.

## Siehe auch:
Für detaillierte Informationen über das 'File'-Modul in Elixir: https://hexdocs.pm/elixir/File.html.
Der Erlang `:file` Modul Dokumentation: http://erlang.org/doc/man/file.html.
Für mehr Beispiele über das Arbeiten mit Dateien in Elixir besuchen Sie: https://elixirschool.com/de/lessons/advanced/otp-concurrency/.