---
title:                "Analysera ett datum från en sträng"
html_title:           "Kotlin: Analysera ett datum från en sträng"
simple_title:         "Analysera ett datum från en sträng"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/powershell/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att tolka ett datum från en sträng innebär att konvertera en textsträng till ett datumformat. Programmerare gör det när de behöver manipulera, jämföra eller göra beräkningar baserat på datumen.

## Hur man gör:

Här är en grundläggande exempel på hur man tolkar ett datum från en sträng i PowerShell:

```PowerShell
# Definiera en sträng med ett datum
$datumStrang = "2021-11-12"

# Använd [datetime]::Parse metod för att tolka datumet
$datum = [datetime]::Parse($datumStrang)

# Visa det tolkade datumet
Write-Output $datum
```

När du kör detta script kommer du att se följande utmatning:

```PowerShell
fre den 12 november 2021 00:00:00
```

## Djupdykning:

PowerShell använder .NET's DateTime-typ för datumhantering. Metoden "[datetime]::Parse" har använts sedan de tidiga versionerna av .NET.

Det finns flera sätt att tolka ett datum från en sträng i PowerShell. Utöver Parse-metoden, kan du även använda ParseExact-metoden, vilket är användbart när du vet exakt formatet av din datumsträng.

När det gäller komplikationerna, om den inmatade strängen inte matchar ett giltigt datumformat kommer [datetime]::Parse att kasta undantag. För att undvika detta kan du använda TryParse-metoden som returnerar ett booleskt värde för att indikera om tolkningen lyckades eller misslyckades.

## Se även:

- [.NET DateTime dokumentation](https://docs.microsoft.com/en-us/dotnet/api/system.datetime?view=net-5.0)