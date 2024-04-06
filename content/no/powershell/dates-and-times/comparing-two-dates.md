---
date: 2024-01-20 17:33:25.831356-07:00
description: "How to: F\xF8r PowerShell, brukte Windows-brukere batch-scripting som\
  \ begrenset dato og tidsh\xE5ndtering. PowerShell introduserte en mer robust DateTime-\u2026"
lastmod: '2024-04-05T22:50:55.033628-06:00'
model: gpt-4-1106-preview
summary: "F\xF8r PowerShell, brukte Windows-brukere batch-scripting som begrenset\
  \ dato og tidsh\xE5ndtering."
title: Sammenlikning av to datoer
weight: 27
---

## How to:
Sammenligne dager direkte:
```PowerShell
$dato1 = Get-Date '2021-05-17'
$dato2 = Get-Date '2022-05-17'

# Sjekk om datoene er de samme
$dato1 -eq $dato2 # Returnerer False

# Sjekk hvilken som er tidligere
$dato1 -lt $dato2 # Returnerer True

# Beregne forskjellen mellom datoene
$forskjell = $dato2 - $dato1
$forskjell.Days # Antall dager forskjell
```

## Deep Dive
Før PowerShell, brukte Windows-brukere batch-scripting som begrenset dato og tidshåndtering. PowerShell introduserte en mer robust DateTime-objektmodell. Det er også andre metoder for dato-sammenligning, inkludert `[datetime]::Compare($dato1, $dato2)` som returnerer -1, 0 eller 1 basert på sammenligningen. Implementasjonsmessig bruker PowerShell .NET's DateTime klasse som grunnlag for behandling av datoer og tider.

## See Also
- Microsoft's dokumentasjon på 'DateTime' klassen: https://docs.microsoft.com/en-us/dotnet/api/system.datetime
- PowerShell's About_Comparison_Operators hjelpeside: https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_comparison_operators
