---
date: 2024-01-26 04:44:14.801756-07:00
description: "Hvordan: PowerShell har ikke innebygd st\xF8tte for komplekse tall,\
  \ s\xE5 du m\xE5 enten lage din egen l\xF8sning eller bruke .NETs `System.Numerics.Complex`."
lastmod: '2024-03-13T22:44:41.010253-06:00'
model: gpt-4-0125-preview
summary: "PowerShell har ikke innebygd st\xF8tte for komplekse tall, s\xE5 du m\xE5\
  \ enten lage din egen l\xF8sning eller bruke .NETs `System.Numerics.Complex`."
title: "\xC5 jobbe med komplekse tall"
weight: 14
---

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
