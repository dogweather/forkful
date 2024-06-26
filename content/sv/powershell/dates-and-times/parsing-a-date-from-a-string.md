---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:15:05.334291-07:00
description: "Hur man g\xF6r: PowerShell g\xF6r det enkelt att tolka datum fr\xE5\
  n str\xE4ngar med sin `Get-Date` cmdlet och typacceleratorn `[datetime]`, som fungerar\
  \ v\xE4l f\xF6r\u2026"
lastmod: '2024-03-13T22:44:38.135199-06:00'
model: gpt-4-0125-preview
summary: "PowerShell g\xF6r det enkelt att tolka datum fr\xE5n str\xE4ngar med sin\
  \ `Get-Date` cmdlet och typacceleratorn `[datetime]`, som fungerar v\xE4l f\xF6\
  r standarddatumformat."
title: "Analysera ett datum fr\xE5n en str\xE4ng"
weight: 30
---

## Hur man gör:
PowerShell gör det enkelt att tolka datum från strängar med sin `Get-Date` cmdlet och typacceleratorn `[datetime]`, som fungerar väl för standarddatumformat. För mer komplexa eller icke-standard datumsträngar, kan metoden `[datetime]::ParseExact` användas för att specificera det exakta formatet.

### Använda `Get-Date` och `[datetime]`:
```powershell
# Enkel konvertering med Get-Date
$stringDate = "2023-04-01"
$date = Get-Date $stringDate
echo $date
```
**Exempelutdata:**
```
Saturday, April 1, 2023 12:00:00 AM
```

```powershell
# Använda typacceleratorn [datetime]
$stringDate = "April 1, 2023"
$date = [datetime]$stringDate
echo $date
```
**Exempelutdata:**
```
Saturday, April 1, 2023 12:00:00 AM
```

### Använda `[datetime]::ParseExact` för icke-standard format:
För format som inte automatiskt känns igen, kan du definiera det exakta formatet för att säkerställa korrekt tolkning.
```powershell
$stringDate = "01-04-2023 14:00"
$format = "dd-MM-yyyy HH:mm"
$culture = [Globalization.CultureInfo]::InvariantCulture
$date = [datetime]::ParseExact($stringDate, $format, $culture)
echo $date
```
**Exempelutdata:**
```
Saturday, April 1, 2023 2:00:00 PM
```

### Använda Tredjepartsbibliotek
Även om PowerShell i sig är ganska kraftfullt för datumparsing, för mycket komplexa scenarier eller ytterligare funktionalitet, kanske du vill utforska .NET-bibliotek som NodaTime, även om PowerShell native kapacitet oftast är tillräcklig för många typiska användningsfall.

```powershell
# Använder NodaTime bara som en illustration, notera att du behöver lägga till biblioteket i ditt projekt
# Install-Package NodaTime -Version 3.0.5
# Använder NodaTime för att tolka ett datum
[string]$stringDate = "2023-04-01T14:00:00Z"
[NodaTime.Instant]::FromDateTimeUtc([datetime]::UtcNow)
[NodaTime.LocalDate]$localDate = [NodaTime.LocalDate]::FromDateTime([datetime]::UtcNow)
echo $localDate
```
**Notera:** Ovanstående kod är en konceptuell illustration. I praktiken, se till att NodaTime är korrekt tillagt i ditt projekt för att typerna och metoderna ska vara tillgängliga.
