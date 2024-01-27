---
title:                "Arbeiten mit CSV-Dateien"
date:                  2024-01-19
html_title:           "Arduino: Arbeiten mit CSV-Dateien"
simple_title:         "Arbeiten mit CSV-Dateien"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/powershell/working-with-csv.md"
---

{{< edit_this_page >}}

## Was & Warum?
CSV steht für "Comma-separated values", also durch Kommas getrennte Werte. Programmierer nutzen CSV, weil es ein einfaches Format ist, um tabellarische Daten zwischen Programmen und Systemen auszutauschen.

## How to:
Hier ist, wie du mit CSVs in PowerShell arbeitest. Schnell und unkompliziert.

### CSV lesen
```PowerShell
$csvDaten = Import-Csv -Path 'C:\BeispielDaten.csv'
$csvDaten
```
Ausgabe:
```
Name       Alter    Stadt
----       -----    ----
Marie      29       Berlin
Tom        35       München
```

### CSV schreiben
```PowerShell
$personen = @(
    [PSCustomObject]@{Name='Marie'; Alter='29'; Stadt='Berlin'},
    [PSCustomObject]@{Name='Tom'; Alter='35'; Stadt='München'}
)
$personen | Export-Csv -Path 'C:\NeueDaten.csv' -NoTypeInformation
```

### CSV filtern
```PowerShell
$csvDaten = Import-Csv -Path 'C:\BeispielDaten.csv'
$csvDaten | Where-Object {$_.Stadt -eq 'München'}
```
Ausgabe:
```
Name       Alter    Stadt
----       -----    ----
Tom        35       München
```

## Deep Dive
CSV ist nicht neu. Es wird seit den frühen 1970er Jahren benutzt und bleibt wegen seiner Einfachheit beliebt. Alternativen wie JSON oder XML sind zwar reichhaltiger in Features, aber CSV ist für simple tabellarische Daten oft ausreichend. PowerShell implementiert CSV-Unterstützung mit Cmdlets wie `Import-Csv` und `Export-Csv`, die Objekte im Speicher nutzen.

## See Also
Weitere Infos und Beispiele findest Du in der offiziellen PowerShell-Dokumentation:

- [Import-Csv (Microsoft Docs)](https://docs.microsoft.com/de-de/powershell/module/microsoft.powershell.utility/import-csv)
- [Export-Csv (Microsoft Docs)](https://docs.microsoft.com/de-de/powershell/module/microsoft.powershell.utility/export-csv)
