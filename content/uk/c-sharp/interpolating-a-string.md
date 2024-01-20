---
title:                "Інтерполяція рядка"
html_title:           "Java: Інтерполяція рядка"
simple_title:         "Інтерполяція рядка"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c-sharp/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Що і чому?

Інтерполяція рядків - це спосіб форматування рядків в C#, який дозволяє укладати змінні прямо в рядок, використовуючи фігурні дужки `{}`. Це дозволяє програмісту легше та продуктивніше створювати рядки, особливо коли рядок залежить від декількох змінних.

## Як це зробити:

```C#
string name = "Peter";
int age = 35;
Console.WriteLine($"Hello, {name}. You are {age} years old.");
```

Виведення:
```
Hello, Peter. You are 35 years old.
```

## Поглиблений огляд

Інтерполяція рядків була введена в C# 6. Вона свого роду замінила `string.Format()`, але та і досі доступна для використання. Інтерполяція рядків простіша у використанні, бо не вимагає порядку розміщення аргументів, як це робить `string.Format()`.

Для інтерполяції рядків компілятор C# використовує метод `String.Format`. Тобто, інтерполяційний рядок створює рядок шаблону і масив аргументів, а потім передає ці два об'єкти в `String.Format` для обробки.

## Дивіться також 

* [MSDN documentation on String Interpolation](https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/tokens/interpolated)
* [MSDN Guide on Formatting types](https://docs.microsoft.com/en-us/dotnet/standard/base-types/formatting-types)