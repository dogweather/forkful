---
title:                "Аналіз дати з рядка"
html_title:           "C++: Аналіз дати з рядка"
simple_title:         "Аналіз дати з рядка"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c-sharp/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Навіщо і чому?

Парсинг дати з рядка – це процес витягування і обробки дати, запакованої у форматі тексту. Програмісти це роблять, щоб використовувати дату в рядку як об'єкт DateTime у комп'ютерних програмах.

## Як це зробити:

C# дуже зручний для парсингу дати з рядка. Методи, які нам знадобляться - `DateTime.Parse()` і `DateTime.TryParse()`. Ось декілька прикладів:

```C#
string strDate = "1/1/2020";

DateTime date1 = DateTime.Parse(strDate);
Console.WriteLine(date1.ToString()); // Виведе: 01.01.2020 00:00:00

bool isParsed = DateTime.TryParse(strDate, out DateTime date2);
Console.WriteLine(isParsed ? date2.ToString() : "Неправильна дата!"); // Виведе: 01.01.2020 00:00:00
```

## Поглиблено:

Парсинг дати з рядка з'явився задовго до C#, розвиваючись з простих алгоритмів обробки тексту. Проте, з появою об'єктно-орієнтованих мов, як C#, парсинг став набагато простішим та безпечнішим завдяки вбудованим методам.

Варто зауважити, що `DateTime.Parse()` може викликати виключення, якщо рядок неможливо обробити як дату, тоді як `DateTime.TryParse()` просто повертає false, що робить його більш безпечним для використання.

Також, важливо пам'ятати, що C# обробляє формати дати відповідно до культурних налаштувань системи. Щоб вказати конкретний формат дати, необхідно використовувати `DateTime.ParseExact()` та `DateTime.TryParseExact()`.

## Дивитеся також:

Поглибити ваши знання можна з допомогою цих матеріалів:

1. [Офіційна документація з DateTime.Parse](https://docs.microsoft.com/en-us/dotnet/api/system.datetime.parse?view=net-5.0)
2. [Офіційна документація з DateTime.TryParse](https://docs.microsoft.com/en-us/dotnet/api/system.datetime.tryparse?view=net-5.0)
3. [Приклади коду з DateTime.ParseExact](https://docs.microsoft.com/en-us/dotnet/api/system.datetime.parseexact?view=net-5.0)
4. [Приклади коду з DateTime.TryParseExact](https://docs.microsoft.com/en-us/dotnet/api/system.datetime.tryparseexact?view=net-5.0)