---
title:                "Округление чисел"
date:                  2024-01-29T00:02:57.418152-07:00
model:                 gpt-4-0125-preview
simple_title:         "Округление чисел"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/powershell/rounding-numbers.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?
Округление чисел заключается в приведении значения к ближайшему целому числу или указанному десятичному разряду. Программисты округляют числа, чтобы упростить данные, повысить их удобочитаемость или удовлетворить определенным математическим требованиям во время вычислений.

## Как это сделать:
В PowerShell есть несколько удобных cmdlet и методов для округления:

- Метод `Round()` из класса Math
```PowerShell
[Math]::Round(15.68) # Округляет до 16
```
- Указание десятичных разрядов:
```PowerShell
[Math]::Round(15.684, 2) # Округляет до 15.68
```
- `Ceiling()` и `Floor()`, для округления всегда вверх или вниз:
```PowerShell
[Math]::Ceiling(15.2) # Округляет в большую сторону до 16
[Math]::Floor(15.9) # Округляет в меньшую сторону до 15
```

## Подробнее
Округление чисел не является новинкой; это используется с древних времен, будучи полезным в торговле, науке и измерении времени. Говоря о PowerShell, `[Math]::Round()` по умолчанию использует "Банковское округление", при котором 0.5 округляется к ближайшему четному числу, сокращая предвзятость в статистических операциях.

Вы не ограничены только методами `[Math]`. Хотите больше контроля? Посмотрите на `[System.Math]::Round(Number, Digits, MidpointRounding)`, где вы можете установить, как будут обрабатываться средние точки: в сторону от нуля или к четному (т.е. Банковское округление).

Еще один аспект: объект `System.Globalization.CultureInfo`. Он помогает с локализацией форматирования и предпочтений округления при работе с международными числами.

## Смотрите также
- Официальная документация Microsoft по методам Math: [Ссылка](https://learn.microsoft.com/en-us/dotnet/api/system.math?view=net-7.0)
- Спецификация округления десятичных чисел в .NET: [Ссылка](https://learn.microsoft.com/en-us/dotnet/api/system.midpointrounding?view=net-7.0)
- Обсуждения округления на StackOverflow: [Ссылка](https://stackoverflow.com/questions/tagged/rounding+powershell)
