---
date: 2024-01-26 04:44:03.285383-07:00
description: "Kuinka: PowerShell ei tue kompleksilukuja suoraan, joten sinun on joko\
  \ kehitett\xE4v\xE4 oma ratkaisusi tai k\xE4ytett\xE4v\xE4 .NET:n `System.Numerics.Complex`."
lastmod: '2024-03-13T22:44:56.774164-06:00'
model: gpt-4-0125-preview
summary: "PowerShell ei tue kompleksilukuja suoraan, joten sinun on joko kehitett\xE4\
  v\xE4 oma ratkaisusi tai k\xE4ytett\xE4v\xE4 .NET:n `System.Numerics.Complex`."
title: "Kompleksilukujen k\xE4sittely"
weight: 14
---

## Kuinka:
PowerShell ei tue kompleksilukuja suoraan, joten sinun on joko kehitettävä oma ratkaisusi tai käytettävä .NET:n `System.Numerics.Complex`.

```PowerShell
# Tehdään kompleksilukuja käyttäen .NET:iä
[Reflection.Assembly]::LoadWithPartialName("System.Numerics") | Out-Null

# Luodaan kompleksilukuja
$complex1 = [System.Numerics.Complex]::new(3, 4) # 3 + 4i
$complex2 = [System.Numerics.Complex]::new(1, 2) # 1 + 2i

# Lisätään kaksi kompleksilukua
$sum = [System.Numerics.Complex]::Add($complex1, $complex2) # 4 + 6i

# Kerrotaan kaksi kompleksilukua
$product = [System.Numerics.Complex]::Multiply($complex1, $complex2) # -5 + 10i

# Näytetään tulokset
"Sum: $sum"
"Product: $product"
```
Tuloste:
```
Summa: (4, 6)
Tulo: (-5, 10)
```

## Syväsukellus
Kompleksiluvut kehitettiin 1500-luvulla ratkaisemaan yhtälöitä, joilla ei ollut ratkaisuja reaalilukujen alueella. Ne ovat nykyään modernin matematiikan kulmakivi.

PowerShellin riippuvuus .NET:istä kompleksilukujen tuessa tarkoittaa, että sen suorituskyky on vankka. Vaihtoehtoja ovat kolmannen osapuolen kirjastot tai muut ohjelmointikielet kuten Python, missä kompleksiluvut ovat natiivi datatyyppi.

## Katso Myös
- [System.Numerics.Complex Rakenne](https://docs.microsoft.com/en-us/dotnet/api/system.numerics.complex)
- [Kompleksilukujen Aritmetiikka Pythonissa](https://docs.python.org/3/library/cmath.html)
