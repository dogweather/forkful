---
title:                "Видобування підрядків"
html_title:           "C#: Видобування підрядків"
simple_title:         "Видобування підрядків"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c-sharp/extracting-substrings.md"
---

{{< edit_this_page >}}

## Чому
Коли ви працюєте з рядками в C#, іноді вам може знадобитися витягнути частину рядка. Наприклад, ви можете бажати отримати тільки телефонний номер з повного рядка або витягувати певні дані зі строки даних. Такі випадки вимагають використання операції витягування підрядка в C#.

## Як це зробити
Щоб витягнути підрядок з рядка в C#, ви можете використовувати методи `Substring()` або `Remove()` класу `String`. Нижче наведені приклади коду та його вихідний результат у форматі Markdown.

```
// Приклад 1: Використання методу Substring()

string fullName = "Іванов Іван Іванович";
string firstName = fullName.Substring(7);
string lastName = fullName.Substring(0, 6);

// Вихідний результат:
// firstName = "Іван Іванович"
// lastName = "Іванов"

```

```
// Приклад 2: Використання методу Remove()

string phoneNumber = "+38 (123) 456-7890";
string cleanNumber = phoneNumber.Remove(0, 3);
// Вихідний результат:
// cleanNumber = "1234567890"
```

## Deep Dive
Операція витягування підрядка в C# працює наступним чином: ви вказуєте початкову позицію та кількість символів, які потрібно витягти з рядка. Метод `Substring()` повертає новий рядок, що містить потрібну частину вихідного рядка. А метод `Remove()` повертає рядок, з якого видалені вказані символи. Також важливо пам'ятати, що індекси символів в C# починаються з нуля.

## Дивіться також
Детальніше про роботу з рядками в C# можна дізнатися за наступними посиланнями:

- [Microsoft Docs: Робота з рядками в C#](https://docs.microsoft.com/uk-ua/dotnet/csharp/programming-guide/strings/)
- [C# Шпаргалка: Рядки](https://overapi.com/csharp-7/strings)
- [C# Курси: Операції з рядками](https://docs.microsoft.com/uk-ua/learn/modules/csharp-string-manipulation/)