---
title:                "Робота з комплексними числами"
aliases: - /uk/powershell/working-with-complex-numbers.md
date:                  2024-01-26T04:44:46.432086-07:00
model:                 gpt-4-0125-preview
simple_title:         "Робота з комплексними числами"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/powershell/working-with-complex-numbers.md"
---

{{< edit_this_page >}}

## Що і чому?
Комплексні числа, ті що мають дійсну та уявну частину (як 3 + 4i), є життєво важливими у таких сферах, як інженерія, фізика та наука про дані. Програмісти використовують їх для симуляцій, обробки сигналів та вирішення специфічних типів математичних задач.

## Як:
У PowerShell немає вбудованої підтримки комплексних чисел, тому ви можете або створити власне рішення, або використати `System.Numerics.Complex` з .NET.

```PowerShell
# Створимо комплексні числа за допомогою .NET
[Reflection.Assembly]::LoadWithPartialName("System.Numerics") | Out-Null

# Створення комплексних чисел
$complex1 = [System.Numerics.Complex]::new(3, 4) # 3 + 4i
$complex2 = [System.Numerics.Complex]::new(1, 2) # 1 + 2i

# Додавання двох комплексних чисел
$sum = [System.Numerics.Complex]::Add($complex1, $complex2) # 4 + 6i

# Множення двох комплексних чисел
$product = [System.Numerics.Complex]::Multiply($complex1, $complex2) # -5 + 10i

# Відображення результатів
"Сума: $sum"
"Добуток: $product"
```
Вивід:
```
Сума: (4, 6)
Добуток: (-5, 10)
```

## Поглиблений розгляд
Комплексні числа були розроблені у 16-му столітті для вирішення рівнянь, які не мали рішень у сфері дійсних чисел. Тепер вони є кутовим каменем сучасної математики.

Залежність PowerShell від .NET для підтримки комплексних чисел означає, що продуктивність є стабільною. Альтернативи включають сторонні бібліотеки або інші мови програмування, як-от Python, де комплексні числа є вродженим типом даних.

## Див. також
- [Структура System.Numerics.Complex](https://docs.microsoft.com/en-us/dotnet/api/system.numerics.complex)
- [Арифметика комплексних чисел в Python](https://docs.python.org/3/library/cmath.html)
