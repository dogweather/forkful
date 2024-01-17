---
title:                "Das Abrufen des aktuellen Datums"
html_title:           "PowerShell: Das Abrufen des aktuellen Datums"
simple_title:         "Das Abrufen des aktuellen Datums"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/powershell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Abrufen des aktuellen Datums ist eine grundlegende und häufig verwendete Funktion in der PowerShell. Programmierer verwenden es, um das aktuelle Datum und die aktuelle Zeit für verschiedene Zwecke zu erhalten, wie z.B. das Erstellen von Protokollen oder das Synchronisieren von Dateien.

## How to:
Hier sind einige Beispiele, die zeigen, wie Sie das aktuelle Datum in der PowerShell abrufen können:

1. Mit dem Befehl `Get-Date` können Sie das aktuelle Datum und die aktuelle Zeit in der Standardformatierung abrufen:
```PowerShell
Get-Date
```

Output:
```
Sonntag, 5. Dezember 2021 22:18:34
```

2. Sie können auch ein bestimmtes Format für das Datum angeben, z.B. `dd.MM.yyyy`, um nur den Tag, den Monat und das Jahr anzuzeigen:
```PowerShell
Get-Date -Format dd.MM.yyyy
```

Output:
```
05.12.2021
```

3. Um das aktuelle Datum zu einem bestimmten Datum hinzu zu addieren, können Sie den Befehl `Get-Date` mit dem Parameter `AddDays` verwenden:
```PowerShell
(Get-Date).AddDays(7)
```

Output:
```
Sonntag, 12. Dezember 2021 22:18:34
```

4. Sie können auch das aktuelle Datum in einem bestimmten Zeitformat abrufen, z.B. `hh:mm:ss tt` für eine 12-Stunden-Zeitangabe mit AM/PM-Anzeige:
```PowerShell
Get-Date -Format hh:mm:ss tt
```

Output:
```
10:18:34 PM
```

## Deep Dive:
- Ursprünglich wurde das Abrufen des aktuellen Datums in der PowerShell mithilfe des .NET Frameworks und der Klasse `DateTime` implementiert. Diese Klasse bietet eine umfangreiche Funktionalität für die Arbeit mit Datum und Zeit.
- Eine Alternative zum `Get-Date` Befehl ist der Befehl `Get-Culture`, der das aktuelle Systemdatum und die aktuelle Systemzeit in einer spezifischen Kultur zurückgibt.
- Die PowerShell bietet auch Befehle wie `New-TimeSpan` und `Measure-Command`, um Zeitintervalle und die Ausführungszeit von Befehlen zu messen.

## Siehe auch:
- [Get-Date Dokumentation auf Microsoft Docs](https://docs.microsoft.com/de-de/powershell/module/microsoft.powershell.utility/get-date)
- [DateTime-Klasse Dokumentation auf Microsoft Docs](https://docs.microsoft.com/de-de/dotnet/api/system.datetime)
- [Microsoft .NET Framework Dokumentation](https://dotnet.microsoft.com/download/dotnet-framework)