---
title:                "Eine temporäre Datei erstellen"
html_title:           "Java: Eine temporäre Datei erstellen"
simple_title:         "Eine temporäre Datei erstellen"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Was & Warum?

Temporäre Dateien zu erstellen kann in bestimmten Programmier-Szenarien sehr nützlich sein. Programmierer erstellen sie meistens, um temporäre Daten zu speichern oder Prozesse zu sequenzieren.

## So geht's:

Hier ist ein einfaches Beispiel zum Erzeugen einer temporären Datei und zum Schreiben von Text in diese Datei:

```Elixir
{:ok, path} = Briefly.create()

File.write!(path, "Hallo, Elixir!")
```

Wenn Sie diesen Code ausführen, erstellt `Briefly.create()` eine temporäre Datei und gibt den Pfad zurück. `File.write!` schreibt dann den String "Hallo, Elixir!" in die Datei.

## Tiefer eintauchen:

Elixir selbst hat keine eingebaute Funktion zum Erzeugen temporärer Dateien. Daher wurde die `Briefly`-Bibliothek entwickelt.

Historisch gesehen war die Erstellung temporärer Dateien eine gängige Praxis, um Speicher im RAM frei zu machen oder sequenzielle Prozesse zu organisieren. Obwohl es heute Alternativen wie In-Memory-Datenbanken gibt, sind temporäre Dateien immer noch eine einfache und effiziente Möglichkeit, diese Aufgaben zu erfüllen.

Die Implementierung des `Briefly.create()` in Elixir ist relativ einfach. Die Funktion verwendet die `:os.tmp_dir` Funktion, um das Verzeichnis für temporäre Dateien zu ermitteln, und erstellt dann eine eindeutige Datei in diesem Verzeichnis.

## Weitere Informationen:

Für eine umfassendere Diskussion zur Erstellung temporärer Dateien in Elixir, schauen Sie bitte auf die [offizielle Dokumentation zur Briefly-Bibliothek](https://hexdocs.pm/briefly/readme.html).

Für Informationen über die Elixir-Datei-API, auf die wir in unserem Code zugegriffen haben, können Sie an der [Elixir-Dateidokumentation](https://hexdocs.pm/elixir/File.html) interessiert sein.