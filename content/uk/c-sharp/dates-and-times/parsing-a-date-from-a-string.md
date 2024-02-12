---
title:                "Розбір дати з рядка"
aliases:
- /uk/c-sharp/parsing-a-date-from-a-string.md
date:                  2024-02-03T19:14:22.321798-07:00
model:                 gpt-4-0125-preview
simple_title:         "Розбір дати з рядка"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c-sharp/parsing-a-date-from-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що і Чому?
Аналіз дати з рядка в C# полягає у конвертації текстових представлень дат та часу у об'єкт `DateTime`. Це необхідно для додатків, які потребують маніпуляції, збереження або відображення дат та часу в різних форматах, таких як додатки для планування, обробники журналів чи будь-які системи, що обробляють введення дати від користувачів або зовнішніх джерел.

## Як:

**Базовий аналіз:**

Методи `DateTime.Parse` та `DateTime.TryParse` є основними варіантами для конвертації рядка у `DateTime`. Ось швидкий приклад:

```csharp
string dateString = "2023-04-12";
DateTime parsedDate;

if (DateTime.TryParse(dateString, out parsedDate))
{
    Console.WriteLine($"Успішно аналізовано: {parsedDate}");
}
else
{
    Console.WriteLine("Не вдалося аналізувати.");
}
// Вивід: Успішно аналізовано: 4/12/2023 12:00:00 AM
```

**Вказівка культури:**

Іноді вам потрібно аналізувати рядок дати, який є у певному форматі культури. Це можна досягнути за допомогою класу `CultureInfo`:

```csharp
using System.Globalization;

string dateString = "12 avril 2023";
var cultureInfo = new CultureInfo("fr-FR");
DateTime parsedDate = DateTime.Parse(dateString, cultureInfo);

Console.WriteLine(parsedDate);
// Вивід: 4/12/2023 12:00:00 AM
```

**Точний аналіз з певним форматом:**

У сценаріях, де дати надходять у певному форматі, який може бути нестандартним, `DateTime.ParseExact` стане в нагоді:

```csharp
string dateString = "Wednesday, 12 April 2023";
string format = "dddd, d MMMM yyyy";
DateTime parsedDate = DateTime.ParseExact(dateString, format, CultureInfo.InvariantCulture);

Console.WriteLine(parsedDate);
// Вивід: 4/12/2023 12:00:00 AM
```

**Використання NodaTime:**

Для ще більш надійного аналізу дати та часу, розгляньте використання популярної сторонньої бібліотеки NodaTime. Вона надає ширший спектр можливостей обробки дати/часу:

```csharp
using NodaTime;
using NodaTime.Text;

var pattern = LocalDatePattern.CreateWithInvariantCulture("yyyy-MM-dd");
var parseResult = pattern.Parse("2023-04-12");

if (parseResult.Success)
{
    LocalDate localDate = parseResult.Value;
    Console.WriteLine(localDate); // 2023-04-12
}
else
{
    Console.WriteLine("Не вдалося аналізувати.");
}
```

NodaTime пропонує широку підтримку часових зон, концепцій періоду та тривалості, а також багато різних календарних систем, роблячи її потужним вибором для складної маніпуляції з датами та часом у .NET додатках.
