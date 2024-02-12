---
title:                "Генерация случайных чисел"
aliases:
- /ru/powershell/generating-random-numbers.md
date:                  2024-01-28T23:58:39.423481-07:00
model:                 gpt-4-0125-preview
simple_title:         "Генерация случайных чисел"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/powershell/generating-random-numbers.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и почему?
Генерация случайных чисел в PowerShell заключается в создании непредсказуемых числовых значений в указанном диапазоне. Программисты используют эту возможность по множеству причин, включая тестирование, моделирование и цели безопасности, где важна непредсказуемость или имитация реальной случайности.

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
