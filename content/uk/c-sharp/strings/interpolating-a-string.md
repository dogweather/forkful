---
title:                "Інтерполяція рядків"
aliases: - /uk/c-sharp/interpolating-a-string.md
date:                  2024-01-20T17:50:44.226602-07:00
model:                 gpt-4-1106-preview
simple_title:         "Інтерполяція рядків"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c-sharp/interpolating-a-string.md"
---

{{< edit_this_page >}}

## What & Why? / Що таке та Навіщо?
Інтерполяція рядків – це спосіб вставлення значень у рядки. Програмісти використовують її для зручного форматування тексту та динамічного створення рядків.

## How to: / Як це зробити:
```C#
string name = "Олексій";
int age = 29;
string message = $"Привіт, {name}! Тобі вже {age} років!";
Console.WriteLine(message);
```
Output / Вивід:
```
Привіт, Олексій! Тобі вже 29 років!
```

## Deep Dive / Поглиблений Розгляд:
В C# інтерполяція рядків з'явилася у версії 6.0 як удосконалення старіших методів форматування, таких як `string.Format`. Інтерполяція дозволяє вставляти вирази безпосередньо в рядки, що робить код чистішим. Наприклад, `string.Format("Привіт, {0}!", name)` замінено на `$"Привіт, {name}!"`. 

Під капотом, компілятор переводить інтерпольований рядок у рядок, сформований через `string.Format`, і вставляє вирази як аргументи. Це може вплинути на продуктивність, особливо при великих обсягах даних, але на практиці такий вплив мінімальний.

Інтерполяція також підтримує форматування та вирівнювання, як наприклад `$"{name,-20}"` додасть до рядку пробіли до двадцяти символів, а `$"{age:D5}"` виведе число з п'ятма цифрами, доповнюючи нулями.

## See Also / Дивіться також:
- [String Interpolation in C#](https://learn.microsoft.com/en-us/dotnet/csharp/language-reference/tokens/interpolated)
- [C# Programming Guide](https://learn.microsoft.com/en-us/dotnet/csharp/programming-guide/)
- [C# String.Format Method](https://learn.microsoft.com/en-us/dotnet/api/system.string.format?view=net-6.0)
