---
date: 2024-01-27 20:34:55.714227-07:00
description: "Jak: PowerShell oferuje prost\u0105 metod\u0119 generowania losowych\
  \ liczb za pomoc\u0105 cmdletu `Get-Random`. Ten cmdlet mo\u017Ce produkowa\u0107\
  \ losowe liczby w domy\u015Blnym\u2026"
lastmod: '2024-03-13T22:44:35.623781-06:00'
model: gpt-4-0125-preview
summary: "PowerShell oferuje prost\u0105 metod\u0119 generowania losowych liczb za\
  \ pomoc\u0105 cmdletu `Get-Random`."
title: Generowanie liczb losowych
weight: 12
---

## Jak:
PowerShell oferuje prostą metodę generowania losowych liczb za pomocą cmdletu `Get-Random`. Ten cmdlet może produkować losowe liczby w domyślnym zakresie lub określonym zakresie.

```PowerShell
# Generowanie losowej liczby między 0 a Int32.MaxValue
$randomNumber = Get-Random
Write-Output $randomNumber
```

Aby określić zakres, użyj parametrów `-Minimum` i `-Maximum`:

```PowerShell
# Generowanie losowej liczby między 1 a 100
$randomNumber = Get-Random -Minimum 1 -Maximum 101
Write-Output $randomNumber
```

Dla większej kontroli, możesz utworzyć obiekt klasy `System.Random`:

```PowerShell
# Używanie System.Random do sekwencji numerów
$rand = New-Object System.Random
foreach ($i in 1..5) {
    $randomNumber = $rand.Next(1, 101)
    Write-Output $randomNumber
}
```

Jeśli potrzebujesz losowego wyboru z tablicy lub kolekcji, `Get-Random` może bezpośrednio wybrać element:

```PowerShell
# Losowy wybór z tablicy
$array = 1..10
$randomItem = Get-Random -InputObject $array
Write-Output $randomItem
```

## Szczegółowa analiza
Cmdlet `Get-Random` w PowerShellu wykorzystuje klasę .NET `System.Random`, aby generować pseudolosowe liczby. Są one "pseudo", ponieważ używają algorytmów do produkcji sekwencji liczb, które tylko wyglądają na losowe. Dla większości aplikacji, ten poziom losowości jest wystarczający. Jednakże, dla przypadków użycia wymagających kryptograficznego bezpieczeństwa, `System.Random` nie jest odpowiedni ze względu na jego przewidywalną naturę.

PowerShell i .NET oferują `System.Security.Cryptography.RNGCryptoServiceProvider` dla kryptograficznej losowości, co jest bardziej odpowiednie do generowania kluczy szyfrowania lub innych operacji wrażliwych na bezpieczeństwo:

```PowerShell
# Kryptograficznie bezpieczne losowe liczby
$rng = [System.Security.Cryptography.RNGCryptoServiceProvider]::new()
$bytes = New-Object byte[] 4
$rng.GetBytes($bytes)
$randomNumber = [BitConverter]::ToInt32($bytes, 0)
Write-Output $randomNumber
```

Podczas gdy `Get-Random` i `System.Random` zaspokajają szeroki zestaw potrzeb związanych z losowością w skryptach i logice aplikacji, istotne jest wybranie odpowiedniego narzędzia do pracy, szczególnie w aplikacjach skoncentrowanych na bezpieczeństwie, gdzie przewidywalność może stanowić podatność.
