---
date: 2024-01-20 17:45:34.468743-07:00
description: "How to: (\u042F\u043A \u0440\u043E\u0431\u0438\u0442\u0438:) ."
lastmod: '2024-03-13T22:44:49.271761-06:00'
model: gpt-4-1106-preview
summary: .
title: "\u0412\u0438\u0434\u0456\u043B\u0435\u043D\u043D\u044F \u043F\u0456\u0434\u0440\
  \u044F\u0434\u043A\u0456\u0432"
weight: 6
---

## How to: (Як робити:)
```C#
string fullText = "Привіт, я програміст із України!";
string extracted = fullText.Substring(8, 12); // Витягуємо "я програміст"

Console.WriteLine(extracted); // Виводить: я програміст
```

За допомогою методу `Substring` можна витягути підстроку, вказавши початковий індекс і довжину. Якщо потрібно витягти все до кінця, довжину можна опустити:

```C#
string endPart = fullText.Substring(24); // Витягуємо "України!"

Console.WriteLine(endPart); // Виводить: України!
```

## Deep Dive (Поглиблене занурення)
В C# витягування підстрок почалося з перших версій мови, метод `Substring` існує давно і часто оновлюється. Альтернативи включають методи `Split`, регулярні вирази і LINQ-операції. Проте `Substring` залишається простим і швидким варіантом.

Працюючи з `Substring`, пам'ятайте, що він створює новий рядок. Це може впливати на продуктивність при великій кількості операцій. Якщо це критично, маніпулюйте символами через `Span<T>` або використовуйте `StringSegment` з бібліотеки Microsoft.Extensions.Primitives для маніпуляцій без створення нових рядків.

## See Also (Дивіться також)
- Метод `Split`: https://docs.microsoft.com/dotnet/api/system.string.split
- Регулярні вирази: https://docs.microsoft.com/dotnet/standard/base-types/regular-expressions
- Офіційна документація `Substring`: https://docs.microsoft.com/dotnet/api/system.string.substring
- LINQ в C#: https://docs.microsoft.com/dotnet/standard/linq/
