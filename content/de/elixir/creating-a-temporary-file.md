---
title:                "Eine temporäre Datei erstellen"
html_title:           "Elixir: Eine temporäre Datei erstellen"
simple_title:         "Eine temporäre Datei erstellen"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Warum

Das Erstellen von temporären Dateien ist in der Programmierung oft nützlich, um Daten zwischenspeichern oder vorübergehend speichern zu können. Es ist besonders hilfreich in Elixir, da es eine funktionale Programmiersprache ist und das Erstellen von Dateien auf eine einfache und effiziente Art und Weise ermöglicht.

## Wie geht das?

Um eine temporäre Datei in Elixir zu erstellen, können wir die Funktion `File.Temp` verwenden. Sie erzeugt eine temporäre Datei mit automatisch generiertem Namen und gibt den Datei-Pfad als Ergebnis zurück. Wir können auch die Option `prefix` verwenden, um einen präfix vor dem Dateinamen anzuhängen.

```Elixir
# Beispiel ohne Präfix
file_path = File.Temp.temp_file()
# Ergebnis: "/tmp/erlF419.temp"

# Beispiel mit Präfix
file_path = File.Temp.temp_file(prefix: "meine_datei_")
# Ergebnis: "/tmp/meine_datei_erlF419.temp"
```

Nachdem die temporäre Datei erstellt wurde, können wir sie nutzen, um Daten zu schreiben oder lesen. Anschließend sollten wir die Datei wieder löschen, um den Speicherplatz frei zu geben.

```Elixir
# Dateiinhalt schreiben
file_path = File.Temp.temp_file()
File.write(file_path, "Hallo Welt")

# Dateiinhalt lesen
File.read(file_path) 
# Ergebnis: {:ok, "Hallo Welt"}

# Datei löschen
File.rm(file_path)
```

## Tiefer tauchen

Die Funktion `File.Temp` verwendet standardmäßig das Verzeichnis `"/tmp"` um die temporären Dateien zu erstellen. Dies kann jedoch mit der Option `dir` geändert werden, um ein anderes Verzeichnis zu verwenden. Außerdem gibt es noch die Option `path`, mit der wir selbst einen vollständigen Dateipfad angeben können.

Eine weitere Möglichkeit, temporäre Dateien zu erstellen, ist die Funktion `File.open_temp/3`. Diese öffnet die Datei automatisch und gibt eine Datei-Deskriptor-Struktur zurück, die wir weiter nutzen können. Am Ende sollte diese Datei ebenfalls gelöscht werden.

```Elixir
# Beispiel mit angegebenem Verzeichnis
file_path = File.Temp.temp_file(dir: "/home/benutzer/tmp/")

# Beispiel mit angegebenem Pfad
file_path = File.Temp.temp_file(path: "/home/benutzer/tmp/datei.txt")
```

## Siehe auch

- [Elixir File Dokumentation](https://hexdocs.pm/elixir/File.html)
- [Offizielle Elixir Website](https://elixir-lang.org/)
- [Einführung in Elixir](https://www.tutorialspoint.com/elixir/index.htm)