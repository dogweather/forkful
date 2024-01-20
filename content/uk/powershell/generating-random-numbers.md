---
title:                "Генерація випадкових чисел"
html_title:           "Java: Генерація випадкових чисел"
simple_title:         "Генерація випадкових чисел"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/powershell/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Генерація випадкових чисел в PowerShell

## Що й чому?
Випадкові числа - це числа, які генеруються програмою в доволі непередбачуваний порядок. Програмісти створюють випадкові числа для забезпечення непередбачуваності, скажімо, при тестуванні коду або роботі з хеш-функціями.

## Як це робиться:

Ось декілька прикладів коду для генерації випадкових чисел у PowerShell.

```PowerShell
# Створення об'єкта Get-Random
$randomNumber = Get-Random
# Виводимо на екран випадкове число
Write-Host "Випадкове число: $randomNumber"
```

Виправімо обмеження для генерації числа від 1 до 100.

```PowerShell
# Генерація випадкового числа від 1 до 100
$randomNumber = Get-Random -Minimum 1 -Maximum 100
# Виводимо на екран випадкове число
Write-Host "Випадкове число: $randomNumber"
```

## Занурення в глибину:

Випадкові числа давно є частиною комп'ютерного кодування. У 1946 році, коли виникли перші комп'ютери, випадкові числа вже використовувалися. 

Альтернативами `Get-Random` є методи .NET `[System.random]` та `[System.Security.Cryptography.RNGCryptoServiceProvider]`. `Get-Random` використовує `(new-object random)`, який є частиною .NET, щоб згенерувати псевдовипадкове число. 

Якщо вам потрібні числа з криптографічної якості, краще використовуйте `[System.Security.Cryptography.RNGCryptoServiceProvider]`. 

## Дивіться також:

1. Довідник по PowerShell: [Get-Random](https://docs.microsoft.com/uk-ua/powershell/module/microsoft.powershell.utility/get-random?view=powershell-7.1)
2. Розгляд методів генерації випадкових чисел: [Random Number Generation in .NET](https://www.dotnetperls.com/random)
3. Криптографічно безпечне генерування випадкових чисел: [RNGCryptoServiceProvider Class](https://docs.microsoft.com/en-us/dotnet/api/system.security.cryptography.rngcryptoserviceprovider?view=net-5.0)