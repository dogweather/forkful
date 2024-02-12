---
title:                "Генерація випадкових чисел"
aliases:
- /uk/powershell/generating-random-numbers/
date:                  2024-01-27T20:35:14.655022-07:00
model:                 gpt-4-0125-preview
simple_title:         "Генерація випадкових чисел"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/powershell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Що і чому?
Генерація випадкових чисел у PowerShell полягає у створенні непередбачуваних числових значень у вказаному діапазоні. Програмісти використовують цю можливість з безлічі причин, включаючи тестування, симуляцію та цілі безпеки, коли непередбачуваність або імітація реальної випадковості є критично важливою.

## Як:
PowerShell пропонує простий підхід для генерації випадкових чисел за допомогою cmdlet `Get-Random`. Цей cmdlet може створювати випадкові числа в межах діапазону за замовчуванням або вказаному діапазоні.

```PowerShell
# Генерація випадкового числа між 0 та Int32.MaxValue
$randomNumber = Get-Random
Write-Output $randomNumber
```

Щоб вказати діапазон, використовуйте параметри `-Minimum` та `-Maximum`:

```PowerShell
# Генерація випадкового числа між 1 та 100
$randomNumber = Get-Random -Minimum 1 -Maximum 101
Write-Output $randomNumber
```

Для більшого контролю, ви можете ініціалізувати об'єкт класу `System.Random`:

```PowerShell
# Використання System.Random для послідовності чисел
$rand = New-Object System.Random
foreach ($i in 1..5) {
    $randomNumber = $rand.Next(1, 101)
    Write-Output $randomNumber
}
```

Якщо вам потрібен випадковий вибір з масиву або колекції, `Get-Random` може безпосередньо вибрати елемент:

```PowerShell
# Випадковий вибір з масиву
$array = 1..10
$randomItem = Get-Random -InputObject $array
Write-Output $randomItem
```

## Поглиблений аналіз
Cmdlet `Get-Random` у PowerShell використовує клас .NET `System.Random` для генерації псевдовипадкових чисел. Вони "псевдо", оскільки використовують алгоритми для створення послідовностей чисел, які лише здаються випадковими. Для більшості застосувань цей рівень випадковості є достатнім. Однак, для сценаріїв, які вимагають криптографічної безпеки, `System.Random` не підходить через його передбачуваний характер.

PowerShell та .NET пропонують `System.Security.Cryptography.RNGCryptoServiceProvider` для криптографічної випадковості, який більше підходить для генерації ключів шифрування або інших операцій, чутливих до безпеки:

```PowerShell
# Криптографічно безпечні випадкові числа
$rng = [System.Security.Cryptography.RNGCryptoServiceProvider]::new()
$bytes = New-Object byte[] 4
$rng.GetBytes($bytes)
$randomNumber = [BitConverter]::ToInt32($bytes, 0)
Write-Output $randomNumber
```

Оскільки `Get-Random` та `System.Random` задовольняють широкий набір потреб для випадковості в сценаріях та логіці додатків, важливо обрати правильний інструмент для завдання, особливо в застосуваннях зорієнтованих на безпеку, де передбачуваність може становити вразливість.
