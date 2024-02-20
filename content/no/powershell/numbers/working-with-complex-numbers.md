---
date: 2024-01-26 04:44:14.801756-07:00
description: "Komplekse tall, de som har en reell del og en imagin\xE6r del (som 3\
  \ + 4i), er essensielle i felt som ingeni\xF8rvitenskap, fysikk og datavitenskap.\u2026"
lastmod: 2024-02-19 22:05:00.278352
model: gpt-4-0125-preview
summary: "Komplekse tall, de som har en reell del og en imagin\xE6r del (som 3 + 4i),\
  \ er essensielle i felt som ingeni\xF8rvitenskap, fysikk og datavitenskap.\u2026"
title: "\xC5 jobbe med komplekse tall"
---

{{< edit_this_page >}}

## Hva og hvorfor?
Komplekse tall, de som har en reell del og en imaginær del (som 3 + 4i), er essensielle i felt som ingeniørvitenskap, fysikk og datavitenskap. Programmerere bruker dem for simuleringer, signalbehandling og løsning av spesifikke typer matematiske problemer.

## Hvordan:
PowerShell har ikke innebygd støtte for komplekse tall, så du må enten lage din egen løsning eller bruke .NETs `System.Numerics.Complex`.

```PowerShell
# La oss lage komplekse tall med .NET
[Reflection.Assembly]::LoadWithPartialName("System.Numerics") | Out-Null

# Opprette komplekse tall
$complex1 = [System.Numerics.Complex]::new(3, 4) # 3 + 4i
$complex2 = [System.Numerics.Complex]::new(1, 2) # 1 + 2i

# Legge til to komplekse tall
$sum = [System.Numerics.Complex]::Add($complex1, $complex2) # 4 + 6i

# Multiplisere to komplekse tall
$product = [System.Numerics.Complex]::Multiply($complex1, $complex2) # -5 + 10i

# Vise resultatene
"Sum: $sum"
"Produkt: $product"
```
Output:
```
Sum: (4, 6)
Produkt: (-5, 10)
```

## Dypdykk
Komplekse tall ble utviklet på 1500-tallet for å løse likninger som ikke hadde løsninger i det reelle tallområdet. De er nå en hjørnestein i moderne matematikk.

PowerShells avhengighet av .NET for støtte av komplekse tall betyr at ytelsen er solid. Alternativer inkluderer tredjepartsbiblioteker eller andre programmeringsspråk som Python, hvor komplekse tall er en innebygd datatype.

## Se også
- [System.Numerics.Complex Struktur](https://docs.microsoft.com/en-us/dotnet/api/system.numerics.complex)
- [Aritmetikk for komplekse tall i Python](https://docs.python.org/3/library/cmath.html)
