---
title:                "Konvertere en dato til en streng"
date:                  2024-01-20T17:37:07.442588-07:00
model:                 gpt-4-1106-preview
simple_title:         "Konvertere en dato til en streng"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/powershell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å konvertere en dato til en streng betyr å endre datotypen fra en dato til tekstformat. Programmerere gjør det for å formatere datoer til lesbare formater for brukere eller for å integrere datoinformasjon i tekstfiler og loggføringer.

## Hvordan:
```PowerShell
# Få dagens dato
$dato = Get-Date

# Konverter dato til en streng
$datoStreng = $dato.ToString("yyyy-MM-dd")
Write-Output $datoStreng

# Ekstrautstyr
$formatertStreng = $dato.ToString("dddd, dd. MMMM yyyy")
Write-Output $formatertStreng

# Resultat
# 2023-04-06
# Torsdag, 06. April 2023
```

## Dypdykk
Konvertering av datoer til strenger har vært en del av programmering siden de første dagene da systemer begynte å håndtere tid og datoer. I PowerShell bruker `Get-Date` cmdleten til å hente systemdatoen og tidspunktet. Metoden `ToString()` kan brukes med egendefinerte formater som styrer den endelige strengens utseende. Alternativt kan du bruke standardformatidentifikatorer eller standard.NET formateringsspesifikasjoner.

Som alternativ til `ToString()`, kan du også bruke `-Format` parameter i `Get-Date` direkte, slik som:
```PowerShell
Get-Date -Format "yyyy-MM-dd"
```
Når du implementerer, vær oppmerksom på kulturelle forskjeller i datoformatering. Bruken av forskjellige kulturer (f.eks. `en-US` versus `nb-NO`) kan endre hvordan datoer blir presentert. I PowerShell kan dette administreres ved å sette `-Culture` parameteren i `ToString()` metoden.

## Se Også
- Microsofts offisielle dokumentasjon om `Get-Date`: https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/get-date
- .NET Standard dato- og tidformater: https://docs.microsoft.com/en-us/dotnet/standard/base-types/standard-date-and-time-format-strings
- PowerShell og kulturinnstillinger: https://docs.microsoft.com/en-us/powershell/scripting/fundamentals/working-with-dates-and-times?view=powershell-7.2