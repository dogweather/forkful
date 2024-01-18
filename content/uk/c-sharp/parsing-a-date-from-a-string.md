---
title:                "Розбір дати з рядка"
html_title:           "C#: Розбір дати з рядка"
simple_title:         "Розбір дати з рядка"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c-sharp/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

Що & Чому?
Парсинг дати з рядка - це процес отримання дати з текстового рядка. Програмісти виконують це, щоб отримати значення дати з рядка і використовувати його для подальших дій, таких як порівняння з іншими датами або форматування.

Як?
Нижче наведені приклади коду та вихідні дані для парсингу дати з текстового рядка в C#:

 ```C#
 // Приклад 1: Парсинг дати з форматом "MM/dd/yyyy"
 string inputDate = "01/20/2021";
 DateTime resultDate = DateTime.ParseExact(inputDate, "MM/dd/yyyy", CultureInfo.InvariantCulture);
 Console.WriteLine(resultDate);
 // Output: 1/20/2021 12:00:00 AM

 // Приклад 2: Парсинг дати з форматом "dd/MM/yyyy HH:mm:ss"
 string inputDate = "20/01/2021 10:25:45";
 DateTime resultDate = DateTime.ParseExact(inputDate, "dd/MM/yyyy HH:mm:ss", CultureInfo.InvariantCulture);
 Console.WriteLine(resultDate);
 // Output: 1/20/2021 10:25:45 AM
 ```

Глибокий дайв
Історичний контекст: раніше програмісти використовували клас `Convert` для парсингу дати з текстового рядка. Але зараз рекомендується використовувати `DateTime.ParseExact`.

Альтернативи: крім `DateTime.ParseExact`, існують інші методи, такі як `DateTime.TryParse` та `DateTime.Parse`, але вони не гарантують коректність парсингу дати з рядка.

Деталі реалізації: метод `DateTime.ParseExact` вимагає вказати формат дати, в якому записана дата у рядку. Якщо цей формат відмінний від представленого в рядку, то парсинг не буде виконано.

Дивись також:
- Документація Microsoft для класу `DateTime`: https://docs.microsoft.com/uk-ua/dotnet/api/system.datetime
- Приклади парсингу дати в інших мовах програмування: https://www.programiz.com/java-programming/examples/parse-date
- Блог девелопера про парсинг дати зі строку: https://devblogs.microsoft.com/dotnet/it-all-starts-with-parse/