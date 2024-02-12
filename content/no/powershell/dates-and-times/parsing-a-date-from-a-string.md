---
title:                "Analysering av en dato fra en streng"
aliases:
- /no/powershell/parsing-a-date-from-a-string.md
date:                  2024-02-03T19:15:12.556349-07:00
model:                 gpt-4-0125-preview
simple_title:         "Analysering av en dato fra en streng"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/powershell/parsing-a-date-from-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Hva & hvorfor?
Å analysere en dato fra en tekststreng handler om å gjenkjenne og konvertere skrevne datoer i tekstform til en datatype dato som PowerShell kan forstå og arbeide med. Programmerere gjør dette for å manipulere, formatere, sammenligne eller beregne datoer, noe som er vanlige oppgaver i skript som håndterer loggfiler, brukerinndata eller databehandling.

## Hvordan:
PowerShell gjør det enkelt å analysere datoer fra tekststrenger med sin `Get-Date` cmdlet og `[datetime]` typeakselerator, som fungerer godt for standard datoformater. For mer komplekse eller ustandardiserte datostrøymer, kan metoden `[datetime]::ParseExact` brukes for å spesifisere det nøyaktige formatet.

### Bruke `Get-Date` og `[datetime]`:
```powershell
# Enkel konvertering med Get-Date
$stringDate = "2023-04-01"
$date = Get-Date $stringDate
echo $date
```
**Eksempelutdata:**
```
Saturday, April 1, 2023 12:00:00 AM
```

```powershell
# Bruke typeakseleratoren [datetime]
$stringDate = "April 1, 2023"
$date = [datetime]$stringDate
echo $date
```
**Eksempelutdata:**
```
Saturday, April 1, 2023 12:00:00 AM
```

### Bruke `[datetime]::ParseExact` for ustandardiserte formater:
For formater som ikke gjenkjennes automatisk, kan du definere det nøyaktige formatet for å sikre riktig analyse.
```powershell
$stringDate = "01-04-2023 14:00"
$format = "dd-MM-yyyy HH:mm"
$culture = [Globalization.CultureInfo]::InvariantCulture
$date = [datetime]::ParseExact($stringDate, $format, $culture)
echo $date
```
**Eksempelutdata:**
```
Saturday, April 1, 2023 2:00:00 PM
```

### Å dra nytte av tredjepartsbiblioteker
Selv om PowerShell i seg selv er ganske kraftfullt for datoomregning, for veldig komplekse scenarioer eller ekstra funksjonalitet, kan du utforske .NET-biblioteker som NodaTime, selv om for mange typiske brukstilfeller, vil PowerShell sine innebygde kapasiteter være tilstrekkelige.

```powershell
# Bruker NodaTime bare som en illustrasjon, merk at du må legge til biblioteket i prosjektet ditt
# Install-Package NodaTime -Version 3.0.5
# Bruker NodaTime for å analysere en dato
[string]$stringDate = "2023-04-01T14:00:00Z"
[NodaTime.Instant]::FromDateTimeUtc([datetime]::UtcNow)
[NodaTime.LocalDate]$localDate = [NodaTime.LocalDate]::FromDateTime([datetime]::UtcNow)
echo $localDate
```
**Eksempel Merk:** Ovenstående kode er en konseptuell illustrasjon. I praksis må du sørge for at NodaTime er korrekt lagt til i prosjektet ditt, for at typene og metodene skal være tilgjengelige.
