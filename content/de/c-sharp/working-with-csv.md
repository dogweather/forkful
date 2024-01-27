---
title:                "Arbeiten mit CSV-Dateien"
date:                  2024-01-19
html_title:           "Arduino: Arbeiten mit CSV-Dateien"
simple_title:         "Arbeiten mit CSV-Dateien"
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/working-with-csv.md"
---

{{< edit_this_page >}}

## Was & Warum?
Arbeiten mit CSV (Comma-Separated Values) bedeutet, Daten in einer simplen Textform zu handhaben, bei der Werte durch Kommas getrennt sind. Programmierer greifen darauf zurück, weil CSV eine leicht lesbare und schreibbare, weit verbreitete Datenformatierung für den Datenaustausch ist.

## How to:
```C#
using System;
using System.Collections.Generic;
using System.IO;

// CSV erstellen und schreiben
var datenListe = new List<string[]>()
{
    new string[] {"Name", "Alter", "Stadt"},
    new string[] {"Max", "25", "Berlin"},
    new string[] {"Anna", "30", "München"}
};

var csvPfad = "beispiel.csv";
using (var sw = new StreamWriter(csvPfad, false))
{
    foreach (var zeile in datenListe)
    {
        var csvZeile = string.Join(",", zeile);
        sw.WriteLine(csvZeile);
    }
}

// CSV lesen
var geleseneDaten = new List<string[]>();
using (var sr = new StreamReader(csvPfad))
{
    string zeile;
    while ((zeile = sr.ReadLine()) != null)
    {
        geleseneDaten.Add(zeile.Split(','));
    }
}

// Erste Zeile ausgeben
foreach (var zelle in geleseneDaten[0])
{
    Console.Write(zelle + " ");
}
```
Sample Output:
```
Name Alter Stadt
```

## Deep Dive
CSV ist seit den frühen Computertagen im Gebrauch und überzeugt durch seine Simplizität. Alternativen wie JSON oder XML bieten strukturierte Datenhaltung und Metadaten, sind aber komplexer. Beim Umgang mit CSV in .NET existieren Bibliotheken wie `CsvHelper`, welche die Implementation vereinfachen und robuste Funktionalitäten, etwa für Serialisierung und Fehlerbehandlung, bieten.

## See Also
- [RFC 4180](https://tools.ietf.org/html/rfc4180) – Das CSV-Standardformat
- [CsvHelper library](https://joshclose.github.io/CsvHelper/) – Beliebte .NET-Bibliothek für CSV
- [Microsoft's guide to file IO](https://docs.microsoft.com/en-us/dotnet/standard/io/) – Microsofts Anleitung für Datei-IO in .NET
