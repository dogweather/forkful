---
title:                "Erstellen einer temporären Datei"
html_title:           "C#: Erstellen einer temporären Datei"
simple_title:         "Erstellen einer temporären Datei"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Erstellen einer temporären Datei ist ein gängiger Vorgang in der Programmierung. Dabei handelt es sich um eine Datei, die nur vorübergehend existiert und für bestimmte Aufgaben oder Prozesse verwendet wird. Programmierer nutzen temporäre Dateien, um Daten zwischenzuspeichern, temporäre Ergebnisse zu speichern oder um Probleme mit Dateiberechtigungen zu umgehen.

## So geht's:

```C#
// Beispiel für das Erstellen einer temporären Datei
var tempFile = Path.GetTempFileName();
// Füge Inhalt zur temporären Datei hinzu
using (var writer = new StreamWriter(tempFile)) {
    writer.WriteLine("Dieser Inhalt wird zur temporären Datei hinzugefügt.");
} 
// Lies den Inhalt der temporären Datei
var contents = File.ReadAllText(tempFile);
Console.WriteLine(contents);
// Ergebnis: Dieser Inhalt wird zur temporären Datei hinzugefügt.
```

## Tiefere Einblicke:

Das Konzept der temporären Dateien wurde erstmals in den 1960er Jahren eingeführt und ist seitdem ein wichtiger Bestandteil der Programmierung. Alternativ zum Erstellen von temporären Dateien können Programmierer auch den Arbeitsspeicher verwenden oder Datenbanken nutzen. Die Implementierung der temporären Dateien kann je nach Betriebssystem variieren.

## Siehe auch:

- [Microsoft Docs: Creating and Using Temporary Files in C#](https://docs.microsoft.com/en-us/dotnet/standard/io/how-to-create-temporary-files)
- [CodeCondo: Temporary Files in C#](https://codecondo.com/tmporary-files-in-c-sharp/)