---
date: 2024-01-27 20:35:14.655022-07:00
description: "\u042F\u043A: PowerShell \u043F\u0440\u043E\u043F\u043E\u043D\u0443\u0454\
  \ \u043F\u0440\u043E\u0441\u0442\u0438\u0439 \u043F\u0456\u0434\u0445\u0456\u0434\
  \ \u0434\u043B\u044F \u0433\u0435\u043D\u0435\u0440\u0430\u0446\u0456\u0457 \u0432\
  \u0438\u043F\u0430\u0434\u043A\u043E\u0432\u0438\u0445 \u0447\u0438\u0441\u0435\u043B\
  \ \u0437\u0430 \u0434\u043E\u043F\u043E\u043C\u043E\u0433\u043E\u044E cmdlet `Get-Random`.\
  \ \u0426\u0435\u0439 cmdlet \u043C\u043E\u0436\u0435 \u0441\u0442\u0432\u043E\u0440\
  \u044E\u0432\u0430\u0442\u0438 \u0432\u0438\u043F\u0430\u0434\u043A\u043E\u0432\u0456\
  \ \u0447\u0438\u0441\u043B\u0430 \u0432 \u043C\u0435\u0436\u0430\u0445\u2026"
lastmod: '2024-03-13T22:44:49.640378-06:00'
model: gpt-4-0125-preview
summary: "PowerShell \u043F\u0440\u043E\u043F\u043E\u043D\u0443\u0454 \u043F\u0440\
  \u043E\u0441\u0442\u0438\u0439 \u043F\u0456\u0434\u0445\u0456\u0434 \u0434\u043B\
  \u044F \u0433\u0435\u043D\u0435\u0440\u0430\u0446\u0456\u0457 \u0432\u0438\u043F\
  \u0430\u0434\u043A\u043E\u0432\u0438\u0445 \u0447\u0438\u0441\u0435\u043B \u0437\
  \u0430 \u0434\u043E\u043F\u043E\u043C\u043E\u0433\u043E\u044E cmdlet `Get-Random`."
title: "\u0413\u0435\u043D\u0435\u0440\u0430\u0446\u0456\u044F \u0432\u0438\u043F\u0430\
  \u0434\u043A\u043E\u0432\u0438\u0445 \u0447\u0438\u0441\u0435\u043B"
weight: 12
---

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
