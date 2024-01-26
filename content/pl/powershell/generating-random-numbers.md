---
title:                "Generowanie liczb losowych"
date:                  2024-01-20T17:50:09.711504-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generowanie liczb losowych"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/powershell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?
Generowanie losowych liczb to po prostu proces tworzenia nieprzewidywalnych wartości. Programiści wykorzystują je w wielu scenariuszach: od testowania oprogramowania przez symulacje po gry i aplikacje z elementami losowości.

## Jak to zrobić:
```PowerShell
# Generowanie pojedynczej losowej liczby między 0 a 100
$random = Get-Random -Minimum 0 -Maximum 101
Write-Host "Wylosowana liczba: $random"

# Generowanie ciągu 5 losowych liczb z zakresu 1 do 50
$randomNumbers = 1..5 | ForEach-Object { Get-Random -Minimum 1 -Maximum 51 }
$randomNumbers -join ', '
```
Przykładowe wyjście:
```
Wylosowana liczba: 28
12, 7, 45, 19, 30
```

## Głębsze zanurzenie
Generowanie losowych liczb nie jest nowe - matematycy zajmowali się tym od wieków. W komputerach stosuje się głównie algorytmy pseudolosowe, które opierają się na wartościach początkowych, zwanymi ziarnami (seeds). W PowerShell `Get-Random` domyślnie używa ziarna opartego o czas systemowy, ale można też określić własne.

Istnieją także inne metody generowania losowości, na przykład za pomocą kryptograficznie bezpiecznych generatorów, które są ważne w zastosowaniach wymagających wysokiego poziomu bezpieczeństwa, jak SSL/TLS. W PowerShell możemy to zrobić przy użyciu `[System.Security.Cryptography.RNGCryptoServiceProvider]`.

Warto również pamiętać, że algorytmy losowości mają swoje ograniczenia i w określonych warunkach mogą być przewidywalne, co bywa wykorzystywane w atakach na systemy.

## Zobacz również
- Dokumentacja PowerShell dla `Get-Random`: https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/get-random
- Wprowadzenie do algorytmów generowania liczb losowych: https://pl.wikipedia.org/wiki/Generator_liczb_losowych
- Informacje o bezpieczeństwie generatorów losowych: https://docs.microsoft.com/en-us/dotnet/api/system.security.cryptography.rngcryptoserviceprovider?view=netframework-4.8
