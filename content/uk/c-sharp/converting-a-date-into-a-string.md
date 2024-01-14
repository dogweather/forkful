---
title:                "C#: Перетворення дати у рядок"
simple_title:         "Перетворення дати у рядок"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c-sharp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

Why: День - це важлива частина життя та програмування. Конвертація дати в рядок є корисним навичкою, яка допоможе вам працювати з датами у ваших проектах та процесах. 

How To: Для конвертації дати в рядок в C#, вам потрібно використати метод `ToString()`. Наприклад:

```C#
using System;

class Program
{
   static void Main()
   {
      DateTime date = new DateTime(2021, 8, 12);
      string stringDate = date.ToString();
      Console.WriteLine(stringDate);
   }
}
```

Вивід програми буде `08/12/2021 00:00:00`.

Також ви можете вказати формат дати в методі `ToString()`, який дасть вам більш контроль над виведеною датою. Наприклад:

```C#
using System;

class Program
{
   static void Main()
   {
      DateTime date = new DateTime(2021, 8, 12);
      string stringDate = date.ToString("yyyy/MM/dd");
      Console.WriteLine(stringDate);
   }
}
```

Вивід програми буде `2021/08/12`.

Для додаткової інформації про форматування дати в C#, ви можете переглянути документацію Microsoft для методу `ToString()`. 

Deep Dive: Конвертація дати в рядок використовується не тільки для виведення дати на екран, але і для збереження дати у файл або передачі її через мережу. Будь-яка операція, яка вимагає використання дати, вимагатиме конвертації дати в рядок.

Крім того, ви можете змінити локалізацію дати за допомогою методу `ToString()`. Наприклад, для виведення дати з українськими назвами місяців, ви можете використати наступний код:

```C#
using System;
using System.Globalization;

class Program
{
   static void Main()
   {
      DateTime date = new DateTime(2021, 8, 12);
      CultureInfo ukCulture = new CultureInfo("uk-UA");
      string stringDate = date.ToString("D", ukCulture);
      Console.WriteLine(stringDate);
   }
}
```

Вивід програми буде "12 серпня 2021 року".

See Also: 

- [Документація Microsoft для методу `ToString()`] (https://docs.microsoft.com/uk-u-UA/dotnet/standard/base-types/custom-numeric-format-strings)
- [Стаття про форматування дати в C#] (https://www.c-sharpcorner.com/uploadfile/mahesh/format-datetime-in-c-sharp/)
- [Детальна стаття про роботу з датами в C#] (https://www.c-sharpcorner.com/UploadFile/mahesh/dates-and-times-in-C-Sharp/)