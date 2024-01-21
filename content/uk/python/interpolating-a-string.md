---
title:                "Інтерполяція рядків"
date:                  2024-01-20T17:51:28.625362-07:00
model:                 gpt-4-1106-preview
simple_title:         "Інтерполяція рядків"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/python/interpolating-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (Що та Чому?)
Стрінг-інтерполяція — це вставка значень у рядки. Програмісти використовують її, щоб зробити код більш читабельним і зменшити кількість помилок при форматуванні тексту.

## How to: (Як це зробити:)
```Python
# f-string (з Python 3.6 та новіше)
name = "Василь"
age = 34
greeting = f"Привіт, {name}! Тобі {age} років."
print(greeting)

# .format() method (старші версії Python)
greeting_format = "Привіт, {}! Тобі {} років.".format(name, age)
print(greeting_format)
```
Обидва підходи виведуть: Привіт, Василь! Тобі 34 років.

## Deep Dive (Занурення в глибину)
Стрінг-інтерполяція була частиною Python уже довгий час. Спочатку ми мали % оператор, відомий як "старий стиль" форматування. З Python 2.6, з'явився метод `.format()`, який був більш гнучким. З Python 3.6, ми отримали f-стрінги, що пропонують ще більшу зручність і швидкість виконання.

Інші мови програмування також використовуют подібні концепції, як, наприклад, String Interpolation у C# або Template Strings у JavaScript.

Для виконання стрінг-інтерполяції Python використовує спеціальний синтаксис, що аналізує рядок і замінює заповнювачі на відповідні значення.

## See Also (Дивіться також)
- Документація Python по f-стрінгам: https://docs.python.org/3/reference/lexical_analysis.html#formatted-string-literals
- PEP 498 про f-стрінги: https://www.python.org/dev/peps/pep-0498/
- Стаття про .format() vs %: https://realpython.com/python-string-formatting/#which-string-formatting-method-should-you-use