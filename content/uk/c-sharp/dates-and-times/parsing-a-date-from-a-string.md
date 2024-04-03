---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:14:22.321798-07:00
description: "\u042F\u043A: **\u0411\u0430\u0437\u043E\u0432\u0438\u0439 \u0430\u043D\
  \u0430\u043B\u0456\u0437:** \u041C\u0435\u0442\u043E\u0434\u0438 `DateTime.Parse`\
  \ \u0442\u0430 `DateTime.TryParse` \u0454 \u043E\u0441\u043D\u043E\u0432\u043D\u0438\
  \u043C\u0438 \u0432\u0430\u0440\u0456\u0430\u043D\u0442\u0430\u043C\u0438 \u0434\
  \u043B\u044F \u043A\u043E\u043D\u0432\u0435\u0440\u0442\u0430\u0446\u0456\u0457\
  \ \u0440\u044F\u0434\u043A\u0430 \u0443 `DateTime`. \u041E\u0441\u044C \u0448\u0432\
  \u0438\u0434\u043A\u0438\u0439 \u043F\u0440\u0438\u043A\u043B\u0430\u0434."
lastmod: '2024-03-13T22:44:49.305032-06:00'
model: gpt-4-0125-preview
summary: "**\u0411\u0430\u0437\u043E\u0432\u0438\u0439 \u0430\u043D\u0430\u043B\u0456\
  \u0437:**\n\n\u041C\u0435\u0442\u043E\u0434\u0438 `DateTime.Parse` \u0442\u0430\
  \ `DateTime.TryParse` \u0454 \u043E\u0441\u043D\u043E\u0432\u043D\u0438\u043C\u0438\
  \ \u0432\u0430\u0440\u0456\u0430\u043D\u0442\u0430\u043C\u0438 \u0434\u043B\u044F\
  \ \u043A\u043E\u043D\u0432\u0435\u0440\u0442\u0430\u0446\u0456\u0457 \u0440\u044F\
  \u0434\u043A\u0430 \u0443 `DateTime`."
title: "\u0420\u043E\u0437\u0431\u0456\u0440 \u0434\u0430\u0442\u0438 \u0437 \u0440\
  \u044F\u0434\u043A\u0430"
weight: 30
---

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
