---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:10:23.285344-07:00
description: "Das Abrufen des aktuellen Datums in PowerShell handelt davon, das aktuelle\
  \ Datum und die Zeit des Systems zu ermitteln. Diese Operation ist grundlegend\u2026"
lastmod: '2024-03-13T22:44:54.114304-06:00'
model: gpt-4-0125-preview
summary: "Das Abrufen des aktuellen Datums in PowerShell handelt davon, das aktuelle\
  \ Datum und die Zeit des Systems zu ermitteln. Diese Operation ist grundlegend\u2026"
title: Den aktuellen Datum abrufen
---

{{< edit_this_page >}}

## Was & Warum?

Das Abrufen des aktuellen Datums in PowerShell handelt davon, das aktuelle Datum und die Zeit des Systems zu ermitteln. Diese Operation ist grundlegend für Aufgaben wie Protokollierung, Zeitmessung oder Entscheidungsfindung auf Basis von Daten. Programmierer nutzen diese Fähigkeit, um Ereignisse zu verfolgen, Aufgaben zu planen und datumsbezogene Logik in Skripten und Anwendungen zu handhaben.

## Wie geht das:

PowerShell bietet unkomplizierte Cmdlets zum Abrufen von Datum und Uhrzeit. Das Cmdlet `Get-Date` ist das primäre Werkzeug zu diesem Zweck. Es kann das vollständige Datum und die Uhrzeit zurückgeben, welche Sie nach Ihren Bedürfnissen formatieren oder manipulieren können.

```powershell
# Das aktuelle Datum und Uhrzeit abrufen
Get-Date
```

**Beispielausgabe:**

```
Dienstag, 5. September 2023 9:46:02 AM
```

Sie können auch das Ausgabeformat anpassen, um nur die Informationen anzuzeigen, die Sie benötigen, wie nur das Datum oder nur die Uhrzeit.

```powershell
# Nur das aktuelle Datum in einem spezifischen Format erhalten
Get-Date -Format "yyyy-MM-dd"
```

**Beispielausgabe:**

```
2023-09-05
```

```powershell
# Nur die aktuelle Uhrzeit erhalten
Get-Date -Format "HH:mm:ss"
```

**Beispielausgabe:**

```
09:46:02
```

### Nutzung der .NET Klasse

PowerShell ermöglicht den direkten Zugriff auf .NET-Klassen und bietet eine alternative Möglichkeit, mit Daten und Zeiten zu arbeiten.

```powershell
# Die aktuelle Datum und Uhrzeit mit der .NET DateTime-Klasse abrufen
[System.DateTime]::Now
```

**Beispielausgabe:**

```
Dienstag, 5. September 2023 9:46:02 AM
```

Für UTC-Zeit:

```powershell
# Die aktuelle UTC Datum und Uhrzeit mit der .NET DateTime-Klasse abrufen
[System.DateTime]::UtcNow
```

**Beispielausgabe:**

```
Dienstag, 5. September 2023 1:46:02 PM
```

Diese Befehle und Klassen bieten leistungsstarke und flexible Optionen für die Arbeit mit Daten und Zeiten in PowerShell, die für viele Skript- und Automatisierungsaufgaben unerlässlich sind.
