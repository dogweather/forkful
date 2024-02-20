---
date: 2024-01-27 20:34:55.714227-07:00
description: "Generowanie losowych liczb w PowerShellu polega na tworzeniu nieprzewidywalnych\
  \ warto\u015Bci liczbowych w okre\u015Blonym zakresie. Programi\u015Bci wykorzystuj\u0105\
  \ t\u0119\u2026"
lastmod: 2024-02-19 22:04:54.764549
model: gpt-4-0125-preview
summary: "Generowanie losowych liczb w PowerShellu polega na tworzeniu nieprzewidywalnych\
  \ warto\u015Bci liczbowych w okre\u015Blonym zakresie. Programi\u015Bci wykorzystuj\u0105\
  \ t\u0119\u2026"
title: Generowanie liczb losowych
---

{{< edit_this_page >}}

## Co i dlaczego?
Generowanie losowych liczb w PowerShellu polega na tworzeniu nieprzewidywalnych wartości liczbowych w określonym zakresie. Programiści wykorzystują tę możliwość z wielu powodów, w tym do testowania, symulacji i celów związanych z bezpieczeństwem, gdzie nieprzewidywalność lub naśladowanie rzeczywistej losowości jest kluczowe.

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
