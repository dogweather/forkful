---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:05:29.497525-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: \u0412 PowerShell \u043D\u0435\u0442 \u0432\u0441\u0442\u0440\u043E\
  \u0435\u043D\u043D\u043E\u0439 \u043F\u043E\u0434\u0434\u0435\u0440\u0436\u043A\u0438\
  \ \u043A\u043E\u043C\u043F\u043B\u0435\u043A\u0441\u043D\u044B\u0445 \u0447\u0438\
  \u0441\u0435\u043B, \u043F\u043E\u044D\u0442\u043E\u043C\u0443 \u0432\u044B \u043B\
  \u0438\u0431\u043E \u0441\u043E\u0437\u0434\u0430\u0435\u0442\u0435 \u0441\u0432\
  \u043E\u0435 \u0441\u043E\u0431\u0441\u0442\u0432\u0435\u043D\u043D\u043E\u0435\
  \ \u0440\u0435\u0448\u0435\u043D\u0438\u0435, \u043B\u0438\u0431\u043E \u0438\u0441\
  \u043F\u043E\u043B\u044C\u0437\u0443\u0435\u0442\u0435\u2026"
lastmod: '2024-03-13T22:44:45.435792-06:00'
model: gpt-4-0125-preview
summary: "\u0412 PowerShell \u043D\u0435\u0442 \u0432\u0441\u0442\u0440\u043E\u0435\
  \u043D\u043D\u043E\u0439 \u043F\u043E\u0434\u0434\u0435\u0440\u0436\u043A\u0438\
  \ \u043A\u043E\u043C\u043F\u043B\u0435\u043A\u0441\u043D\u044B\u0445 \u0447\u0438\
  \u0441\u0435\u043B, \u043F\u043E\u044D\u0442\u043E\u043C\u0443 \u0432\u044B \u043B\
  \u0438\u0431\u043E \u0441\u043E\u0437\u0434\u0430\u0435\u0442\u0435 \u0441\u0432\
  \u043E\u0435 \u0441\u043E\u0431\u0441\u0442\u0432\u0435\u043D\u043D\u043E\u0435\
  \ \u0440\u0435\u0448\u0435\u043D\u0438\u0435, \u043B\u0438\u0431\u043E \u0438\u0441\
  \u043F\u043E\u043B\u044C\u0437\u0443\u0435\u0442\u0435 `System.Numerics.Complex`\
  \ \u0438\u0437 .NET."
title: "\u0420\u0430\u0431\u043E\u0442\u0430 \u0441 \u043A\u043E\u043C\u043F\u043B\
  \u0435\u043A\u0441\u043D\u044B\u043C\u0438 \u0447\u0438\u0441\u043B\u0430\u043C\u0438"
weight: 14
---

## Как это сделать:
В PowerShell нет встроенной поддержки комплексных чисел, поэтому вы либо создаете свое собственное решение, либо используете `System.Numerics.Complex` из .NET.

```PowerShell
# Давайте создадим комплексные числа с помощью .NET
[Reflection.Assembly]::LoadWithPartialName("System.Numerics") | Out-Null

# Создаем комплексные числа
$complex1 = [System.Numerics.Complex]::new(3, 4) # 3 + 4i
$complex2 = [System.Numerics.Complex]::new(1, 2) # 1 + 2i

# Складываем два комплексных числа
$sum = [System.Numerics.Complex]::Add($complex1, $complex2) # 4 + 6i

# Умножаем два комплексных числа
$product = [System.Numerics.Complex]::Multiply($complex1, $complex2) # -5 + 10i

# Отображаем результаты
"Сумма: $sum"
"Произведение: $product"
```
Вывод:
```
Сумма: (4, 6)
Произведение: (-5, 10)
```

## Глубокое погружение
Комплексные числа были разработаны в XVI веке для решения уравнений, которые не имели решений в области действительных чисел. Сегодня они являются основополагающим элементом современной математики.

Зависимость PowerShell от .NET для поддержки комплексных чисел означает, что производительность на высоком уровне. Альтернативы включают сторонние библиотеки или другие языки программирования, такие как Python, где комплексные числа являются встроенным типом данных.

## См. также
- [Структура System.Numerics.Complex](https://docs.microsoft.com/ru-ru/dotnet/api/system.numerics.complex)
- [Арифметика комплексных чисел в Python](https://docs.python.org/3/library/cmath.html)
