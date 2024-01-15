---
title:                "Капіталізація рядка"
html_title:           "C#: Капіталізація рядка"
simple_title:         "Капіталізація рядка"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c-sharp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Причина

Існує багато ситуацій, коли потрібно зробити першу літеру кожного слова у строковому значенні великою, наприклад, для коректного форматування у імені особи або заголовка статті. Дізнаємося, як це зробити за допомогою мови програмування C#.

## Як це зробити

```C#
string input = "зробити строку великою першу літеру кожного слова";
string output = CultureInfo.CurrentCulture.TextInfo.ToTitleCase(input.ToLower());
Console.WriteLine(output);

// Output: Зробити Строку Великою Першу Літеру Кожного Слова
```

У цьому прикладі ми використовуємо метод `ToTitleCase` з `TextInfo` класу, що надає мова програмування C#. Цей метод першим кроком перетворює всі символи у нижній регістр, а потім першу літеру кожного слова робить великою.

Цей метод також враховує різні мовні правила, тому результат буде коректним для будь-якої мови, яку використовуєте.

## Глибоке занурення

Ви можете додатково налаштувати результат, використовуючи `TextInfo` клас. Наприклад, ви можете встановити спеціальні правила для перших літер у непевних словах, наприклад, назвах місць або прізвищах.

Також можна використовувати цей метод для перших літер усіх слів, а не лише у першому слові, вказуючи `CompareOptions` параметр.

Детальніше про цей метод та інші можливості мови програмування C# можна дізнатися у [документації Microsoft](https://docs.microsoft.com/en-us/dotnet/api/system.globalization.textinfo.totitlecase?view=net-5.0).

## Дивіться також

- [Документація Microsoft про `TextInfo` клас](https://docs.microsoft.com/en-us/dotnet/api/system.globalization.textinfo?view=net-5.0)
- [Бібліотека для роботи з міжнародними текстами в C#](https://github.com/sillsdev/libpalaso/wiki/Localization-and-Internationalization)
- [Порівняння мовних правил у різних країнах](https://en.wikipedia.org/wiki/Title_case#Language-specific_capitalization_rules)