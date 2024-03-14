---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:15:03.318881-07:00
description: "Das Parsen eines Datums aus einem String bedeutet, geschriebene Daten\
  \ in Textform zu erkennen und in einen Datentyp umzuwandeln, den PowerShell verstehen\u2026"
lastmod: '2024-03-13T22:44:54.113247-06:00'
model: gpt-4-0125-preview
summary: "Das Parsen eines Datums aus einem String bedeutet, geschriebene Daten in\
  \ Textform zu erkennen und in einen Datentyp umzuwandeln, den PowerShell verstehen\u2026"
title: Einen Datum aus einem String analysieren
---

{{< edit_this_page >}}

## Was & Warum?
Das Parsen eines Datums aus einem String bedeutet, geschriebene Daten in Textform zu erkennen und in einen Datentyp umzuwandeln, den PowerShell verstehen und mit dem es arbeiten kann. Programmierer tun dies, um Daten zu manipulieren, zu formatieren, zu vergleichen oder zu berechnen, was bei Skripten, die sich mit Logdateien, Benutzereingaben oder der Datenverarbeitung befassen, häufig vorkommt.

## Wie geht das:
PowerShell macht das Parsen von Daten aus Strings unkompliziert mit seinem `Get-Date`-Cmdlet und der Typbeschleunigung `[datetime]`, die gut für standardisierte Datumsformate funktionieren. Für komplexere oder nicht standardisierte Datumsstrings kann die Methode `[datetime]::ParseExact` verwendet werden, um das genaue Format festzulegen.

### Verwendung von `Get-Date` und `[datetime]`:
```powershell
# Einfache Konvertierung mit Get-Date
$stringDate = "2023-04-01"
$date = Get-Date $stringDate
echo $date
```
**Beispielausgabe:**
```
Samstag, 1. April 2023 00:00:00
```

```powershell
# Verwendung des Typbeschleunigers [datetime]
$stringDate = "April 1, 2023"
$date = [datetime]$stringDate
echo $date
```
**Beispielausgabe:**
```
Samstag, 1. April 2023 00:00:00
```

### Verwendung von `[datetime]::ParseExact` für nicht standardisierte Formate:
Für Formate, die nicht automatisch erkannt werden, können Sie das genaue Format definieren, um eine korrekte Analyse sicherzustellen.
```powershell
$stringDate = "01-04-2023 14:00"
$format = "dd-MM-yyyy HH:mm"
$culture = [Globalization.CultureInfo]::InvariantCulture
$date = [datetime]::ParseExact($stringDate, $format, $culture)
echo $date
```
**Beispielausgabe:**
```
Samstag, 1. April 2023 14:00:00
```

### Einsatz von Drittanbieter-Bibliotheken
Obwohl PowerShell selbst ziemlich leistungsfähig für das Parsen von Daten ist, könnte man für sehr komplexe Szenarien oder zusätzliche Funktionalitäten .NET-Bibliotheken wie NodaTime erkunden, obwohl PowerShell's native Fähigkeiten für viele typische Anwendungsfälle ausreichen werden.

```powershell
# Verwendung von NodaTime nur als Illustration, beachten Sie, dass Sie die Bibliothek zu Ihrem Projekt hinzufügen müssen
# Install-Package NodaTime -Version 3.0.5
# Verwendung von NodaTime zum Parsen eines Datums
[string]$stringDate = "2023-04-01T14:00:00Z"
[NodaTime.Instant]::FromDateTimeUtc([datetime]::UtcNow)
[NodaTime.LocalDate]$localDate = [NodaTime.LocalDate]::FromDateTime([datetime]::UtcNow)
echo $localDate
```
**Wichtiger Hinweis:** Der obige Code dient nur als konzeptionelle Illustration. In der Praxis stellen Sie sicher, dass NodaTime korrekt zu Ihrem Projekt hinzugefügt wird, damit die Typen und Methoden verfügbar sind.
