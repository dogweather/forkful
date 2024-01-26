---
title:                "Tolke en dato fra en streng"
date:                  2024-01-20T15:37:59.462583-07:00
html_title:           "Arduino: Tolke en dato fra en streng"
simple_title:         "Tolke en dato fra en streng"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/powershell/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Parsing av dato fra en streng betyr å konvertere tekst til et datoobjekt. Dette gjør programmerere for å enkelt manipulere og sammenligne datoer, og for å integrere brukerinput i applikasjoner.

## Slik gjør du det:

For å konvertere en streng til en dato i PowerShell, bruk `Get-Date`:

```PowerShell
$datoStreng = "2023-04-01"
$datoObjekt = Get-Date $datoStreng
Write-Output $datoObjekt
```

Resultatet blir et datoobjekt:
```
lørdag 1. april 2023 00:00:00
```

Du kan også spesifisere formatet:

```PowerShell
$datoStreng = "01-04-2023 14:00"
$format = "dd-MM-yyyy HH:mm"
$datoObjekt = Get-Date $datoStreng -Format $format
Write-Output $datoObjekt
```

Viser datoen gitt formatet:
```
01-04-2023 14:00
```

## Forståelsen Bak

Før PowerShell hadde egne cmdlets, måtte man stole på .NET klasser for dato-parsing. `Get-Date` er nå standardcmdlet for å håndtere datoer, som forenkler mange oppgaver. Alternativer inkluderer direkte bruk av .NET metoder som `[datetime]::Parse()`, som kan gi mer kontroll med komplekse formatkrav. PowerShell håndterer også automatisk kulturelle forskjeller i datoformat, noe som er viktig i et globalt programmeringsmiljø.

## Se Også

- [PowerShell dokumentasjon for `Get-Date`](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/get-date?view=powershell-7.1)
- [.NET dokumentasjon for DateTime](https://docs.microsoft.com/en-us/dotnet/api/system.datetime?view=netframework-4.8)
- [Om Parsing Dates i PowerShell på `SS64.com`](https://ss64.com/ps/syntax-dateformats.html)
