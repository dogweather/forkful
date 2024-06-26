---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 23:58:39.423481-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: PowerShell \u043F\u0440\u0435\u0434\u043B\u0430\u0433\u0430\u0435\u0442\
  \ \u043F\u0440\u043E\u0441\u0442\u043E\u0439 \u043F\u043E\u0434\u0445\u043E\u0434\
  \ \u043A \u0433\u0435\u043D\u0435\u0440\u0430\u0446\u0438\u0438 \u0441\u043B\u0443\
  \u0447\u0430\u0439\u043D\u044B\u0445 \u0447\u0438\u0441\u0435\u043B \u0441 \u043F\
  \u043E\u043C\u043E\u0449\u044C\u044E \u043A\u043E\u043C\u0430\u043D\u0434\u043B\u0435\
  \u0442\u0430 `Get-Random`. \u042D\u0442\u043E\u0442 \u043A\u043E\u043C\u0430\u043D\
  \u0434\u043B\u0435\u0442 \u043C\u043E\u0436\u0435\u0442 \u043F\u0440\u043E\u0438\
  \u0437\u0432\u043E\u0434\u0438\u0442\u044C\u2026"
lastmod: '2024-03-13T22:44:45.439531-06:00'
model: gpt-4-0125-preview
summary: "PowerShell \u043F\u0440\u0435\u0434\u043B\u0430\u0433\u0430\u0435\u0442\
  \ \u043F\u0440\u043E\u0441\u0442\u043E\u0439 \u043F\u043E\u0434\u0445\u043E\u0434\
  \ \u043A \u0433\u0435\u043D\u0435\u0440\u0430\u0446\u0438\u0438 \u0441\u043B\u0443\
  \u0447\u0430\u0439\u043D\u044B\u0445 \u0447\u0438\u0441\u0435\u043B \u0441 \u043F\
  \u043E\u043C\u043E\u0449\u044C\u044E \u043A\u043E\u043C\u0430\u043D\u0434\u043B\u0435\
  \u0442\u0430 `Get-Random`."
title: "\u0413\u0435\u043D\u0435\u0440\u0430\u0446\u0438\u044F \u0441\u043B\u0443\u0447\
  \u0430\u0439\u043D\u044B\u0445 \u0447\u0438\u0441\u0435\u043B"
weight: 12
---

## Как это сделать:
PowerShell предлагает простой подход к генерации случайных чисел с помощью командлета `Get-Random`. Этот командлет может производить случайные числа в пределах диапазона по умолчанию или указанного диапазона.

```PowerShell
# Генерация случайного числа между 0 и Int32.MaxValue
$randomNumber = Get-Random
Write-Output $randomNumber
```

Чтобы указать диапазон, используйте параметры `-Minimum` и `-Maximum`:

```PowerShell
# Генерация случайного числа между 1 и 100
$randomNumber = Get-Random -Minimum 1 -Maximum 101
Write-Output $randomNumber
```

Для большего контроля можно создать объект класса `System.Random`:

```PowerShell
# Использование System.Random для последовательности чисел
$rand = New-Object System.Random
foreach ($i in 1..5) {
    $randomNumber = $rand.Next(1, 101)
    Write-Output $randomNumber
}
```

Если вам нужен случайный выбор из массива или коллекции, `Get-Random` может напрямую выбрать элемент:

```PowerShell
# Случайный выбор из массива
$array = 1..10
$randomItem = Get-Random -InputObject $array
Write-Output $randomItem
```

## Глубокое погружение
Командлет `Get-Random` в PowerShell использует под капотом класс .NET `System.Random` для генерации псевдослучайных чисел. Они являются "псевдо", потому что используют алгоритмы для производства последовательностей чисел, которые только кажутся случайными. Для большинства приложений этого уровня случайности достаточно. Однако для случаев, требующих криптографической безопасности, `System.Random` не подходит из-за его предсказуемости.

PowerShell и .NET предлагают `System.Security.Cryptography.RNGCryptoServiceProvider` для криптографической случайности, который более подходит для генерации ключей шифрования или других операций, связанных с безопасностью:

```PowerShell
# Криптографически безопасные случайные числа
$rng = [System.Security.Cryptography.RNGCryptoServiceProvider]::new()
$bytes = New-Object byte[] 4
$rng.GetBytes($bytes)
$randomNumber = [BitConverter]::ToInt32($bytes, 0)
Write-Output $randomNumber
```

Хотя `Get-Random` и `System.Random` удовлетворяют широкому набору потребностей в случайности при написании скриптов и логике приложений, важно выбирать правильный инструмент для работы, особенно в приложениях, ориентированных на безопасность, где предсказуемость может представлять уязвимость.
