---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:20:36.688714-07:00
description: "Das Arbeiten mit CSV-Dateien (Comma-Separated Values, also durch Kommas\
  \ getrennte Werte) ist eine g\xE4ngige Aufgabe beim Verwalten und Manipulieren von\u2026"
lastmod: '2024-02-25T18:49:51.177776-07:00'
model: gpt-4-0125-preview
summary: "Das Arbeiten mit CSV-Dateien (Comma-Separated Values, also durch Kommas\
  \ getrennte Werte) ist eine g\xE4ngige Aufgabe beim Verwalten und Manipulieren von\u2026"
title: Arbeiten mit CSV
---

{{< edit_this_page >}}

## Was & Warum?

Das Arbeiten mit CSV-Dateien (Comma-Separated Values, also durch Kommas getrennte Werte) ist eine gängige Aufgabe beim Verwalten und Manipulieren von Daten in strukturierter, tabellarischer Form. Programmierer führen diese Operation oft durch, um Daten für verschiedene Anwendungen effizient zu importieren, exportieren oder zu manipulieren, wie etwa Datenanalyse, Berichterstattung oder sogar das Betreiben von Webanwendungen.

## Wie geht das:

### Eine CSV-Datei lesen

Um aus einer CSV-Datei zu lesen, verwenden Sie das Cmdlet `Import-Csv`. Dieses Cmdlet liest die Datei und konvertiert sie in benutzerdefinierte PowerShell-Objekte für jede Zeile.

```powershell
# Importieren einer CSV-Datei
$data = Import-Csv -Path "C:\Data\users.csv"
# Anzeigen des Inhalts
$data
```

**Beispielausgabe:**

```
Name    Alter    Stadt
----    -----    -----
John    23       New York
Doe     29       Los Angeles
```

### In eine CSV-Datei schreiben

Umgekehrt wird zum Schreiben von Daten in eine CSV-Datei das Cmdlet `Export-Csv` verwendet. Dieses Cmdlet nimmt Eingabeobjekte und konvertiert sie in ein CSV-Format.

```powershell
# Erstellen eines Objekts zum Exportieren
$users = @(
    [PSCustomObject]@{Name='John'; Alter='23'; Stadt='New York'},
    [PSCustomObject]@{Name='Doe'; Alter='29'; Stadt='Los Angeles'}
)

# Exportieren in eine CSV-Datei
$users | Export-Csv -Path "C:\Data\new_users.csv" -NoTypeInformation
```

Nach der Ausführung wird eine Datei namens `new_users.csv` mit den bereitgestellten Daten erstellt.

### Filtern und Manipulieren von CSV-Inhalten

Um die Daten aus einer CSV-Datei zu filtern oder zu manipulieren, verwenden Sie die Objektmanipulationsfähigkeiten von PowerShell. Um zum Beispiel nur Benutzer über einem bestimmten Alter und aus einer bestimmten Stadt auszuwählen:

```powershell
# Importieren und Filtern von Daten
$filteredData = Import-Csv -Path "C:\Data\users.csv" | Where-Object {
    $_.Alter -gt 25 -and $_.Stadt -eq 'Los Angeles'
}

# Anzeigen der gefilterten Daten
$filteredData
```

**Beispielausgabe:**

```
Name    Alter    Stadt
----    -----    -----
Doe     29       Los Angeles
```

### Verwendung von Drittanbieter-Bibliotheken

Obwohl die nativen Cmdlets von PowerShell für übliche Aufgaben normalerweise ausreichen, könnten komplexere Operationen von Drittanbieter-Bibliotheken oder -Tools profitieren. Jedoch bieten für die Standardmanipulation von CSVs, wie das Lesen, Schreiben, Filtern oder Sortieren, die integrierten Cmdlets von PowerShell wie `Import-Csv` und `Export-Csv` in der Regel eine robuste Funktionalität, ohne dass zusätzliche Bibliotheken benötigt werden.
