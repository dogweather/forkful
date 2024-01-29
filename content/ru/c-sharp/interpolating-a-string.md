---
title:                "Интерполяция строки"
date:                  2024-01-28T23:58:58.072869-07:00
model:                 gpt-4-0125-preview
simple_title:         "Интерполяция строки"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/c-sharp/interpolating-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и почему?
Интерполяция строк позволяет создавать строки, используя встроенные выражения. Это делает код читаемым и форматирование простым.

## Как:
```C# 
string name = "Alex";
int age = 29;
string greeting = $"Привет, {name}! Тебе {age} лет.";
Console.WriteLine(greeting);
```
Вывод:
```
Привет, Alex! Тебе 29 лет.
```

## Подробнее
Интерполяция строк была введена в C# 6, повышая удобство форматирования строк по сравнению со старым методом `String.Format`. Исторически можно было увидеть что-то вроде этого:

```C# 
string greeting = string.Format("Привет, {0}! Тебе {1} лет.", name, age);
```

Интерполяция в C# — это синтаксический сахар, который компилятор преобразует в вызов `String.Format`. Она работает путем разбора интерполированной строки и замены выражений в фигурных скобках `{}` строковыми представлениями результатов этих выражений. Внутренне она использует `StringBuilder`, что делает её более эффективной, чем конкатенацию в циклах.

Альтернатива интерполяции строк - это оператор плюс (`+`) для конкатенации, но это может быстро стать нечитаемым и громоздким, и часто более подвержено ошибкам.

```C# 
string greeting = "Привет, " + name + "! Тебе " + age + " лет.";
```

Учитывая эти альтернативы, интерполяция строк часто является предпочтительным выбором за её ясность и эффективность в большинстве сценариев.

## Смотрите также
Для получения дополнительной информации о форматировании строк в C#, MSDN будет вашим другом:
- [Интерполяция строк](https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/tokens/interpolated)
- [String.Format](https://docs.microsoft.com/en-us/dotnet/api/system.string.format?view=net-6.0)
- [StringBuilder](https://docs.microsoft.com/en-us/dotnet/api/system.text.stringbuilder?view=net-6.0)
