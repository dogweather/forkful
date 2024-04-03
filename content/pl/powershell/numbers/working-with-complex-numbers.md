---
date: 2024-01-26 04:44:14.642879-07:00
description: "Jak to zrobi\u0107: PowerShell nie posiada wbudowanego wsparcia dla\
  \ liczb zespolonych, wi\u0119c albo tworzysz w\u0142asne rozwi\u0105zanie, albo\
  \ korzystasz z\u2026"
lastmod: '2024-03-13T22:44:35.621936-06:00'
model: gpt-4-0125-preview
summary: "PowerShell nie posiada wbudowanego wsparcia dla liczb zespolonych, wi\u0119\
  c albo tworzysz w\u0142asne rozwi\u0105zanie, albo korzystasz z `System.Numerics.Complex`\
  \ z .NET."
title: Praca z liczbami zespolonymi
weight: 14
---

## Jak to zrobić:
PowerShell nie posiada wbudowanego wsparcia dla liczb zespolonych, więc albo tworzysz własne rozwiązanie, albo korzystasz z `System.Numerics.Complex` z .NET.

```PowerShell
# Tworzymy liczby zespolone przy pomocy .NET
[Reflection.Assembly]::LoadWithPartialName("System.Numerics") | Out-Null

# Tworzenie liczb zespolonych
$complex1 = [System.Numerics.Complex]::new(3, 4) # 3 + 4i
$complex2 = [System.Numerics.Complex]::new(1, 2) # 1 + 2i

# Dodawanie dwóch liczb zespolonych
$sum = [System.Numerics.Complex]::Add($complex1, $complex2) # 4 + 6i

# Mnożenie dwóch liczb zespolonych
$product = [System.Numerics.Complex]::Multiply($complex1, $complex2) # -5 + 10i

# Wyświetlanie wyników
"Suma: $sum"
"Iloczyn: $product"
```
Wynik:
```
Suma: (4, 6)
Iloczyn: (-5, 10)
```

## Wgłębiając się
Liczby zespolone zostały opracowane w XVI wieku, aby rozwiązać równania, które nie miały rozwiązań w dziedzinie liczb rzeczywistych. Obecnie są one kamieniem węgielnym nowoczesnej matematyki.

Oparcie PowerShell na .NET dla wsparcia liczb zespolonych oznacza, że wydajność jest solidna. Alternatywy to biblioteki stron trzecich lub inne języki programowania, takie jak Python, gdzie liczby zespolone są natywnym typem danych.

## Zobacz również
- [Struktura System.Numerics.Complex](https://docs.microsoft.com/en-us/dotnet/api/system.numerics.complex)
- [Arytmetyka liczb zespolonych w Pythonie](https://docs.python.org/3/library/cmath.html)
