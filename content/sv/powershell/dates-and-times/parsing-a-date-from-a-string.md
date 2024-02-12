---
title:                "Analysera ett datum från en sträng"
aliases:
- sv/powershell/parsing-a-date-from-a-string.md
date:                  2024-02-03T19:15:05.334291-07:00
model:                 gpt-4-0125-preview
simple_title:         "Analysera ett datum från en sträng"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/powershell/parsing-a-date-from-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Vad & Varför?
Att tolka ett datum från en sträng handlar om att känna igen och konvertera skrivna datum i textform till en datatyp som PowerShell kan förstå och arbeta med. Programmerare gör detta för att manipulera, formatera, jämföra eller beräkna datum, vilket är vanliga uppgifter i skript som hanterar loggfiler, användarinmatning eller databehandling.

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
