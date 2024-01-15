---
title:                "Порівняння двох дат"
html_title:           "C#: Порівняння двох дат"
simple_title:         "Порівняння двох дат"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c-sharp/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Чому

Порівняння двох дат - це важлива задача у багатьох програмах. Наприклад, це може знадобитися для перевірки терміну дії товару або для обліку вікових категорій користувачів. Але чому саме у C# потрібно порівнювати дати? Давайте докладніше розберемося.

## Як це зробити

Кодування порівняння двох дат в C# - це досить просте завдання. Для цього використовується клас **DateTime** і його методи. Давайте розглянемо кілька прикладів коду та відповідні виходи.

```C#
// Порівняння дати з поточною датою
DateTime date = new DateTime(2021, 01, 01);
if (date > DateTime.Today)
{
    Console.WriteLine("Дата ще не наставала.");
}
else if (date == DateTime.Today)
{
    Console.WriteLine("Сьогодні точно ця дата!");
}
else
{
    Console.WriteLine("Ця дата вже минула.");
}
```

Виходи:

```
Дата ще не наставала.
```

```C#
// Порівняння двох дат
DateTime firstDate = new DateTime(2021, 01, 01);
DateTime secondDate = new DateTime(2021, 02, 01);
if (firstDate < secondDate)
{
    Console.WriteLine("Перша дата раніше другої.");
}
else if (firstDate > secondDate)
{
    Console.WriteLine("Перша дата пізніше другої.");
}
else
{
    Console.WriteLine("Ці дати однакові.");
}
```

Виходи:

```
Перша дата раніше другої.
```

```C#
// Порівняння дат по частинам (рік, місяць, день)
DateTime firstDate = new DateTime(2021, 01, 01);
DateTime secondDate = new DateTime(2021, 02, 01);
if (firstDate.Year == secondDate.Year)
{
    Console.WriteLine("Роки однакові.");
    if (firstDate.Month == secondDate.Month)
    {
        Console.WriteLine("Місяці однакові.");
        if (firstDate.Day == secondDate.Day)
        {
            Console.WriteLine("Дні однакові.");
        }
    }
}
```

Виходи:

```
Роки однакові.
Місяці однакові.
```

## Вдавайся глибше

Клас **DateTime** застосовується для збереження інформації про дату і час. Також існує клас **TimeSpan**, який використовується для роботи з періодами часу. Для порівняння періодів часу, наприклад, днів, можна використовувати метод **Difference**:

```C#
// Порівняння періоду часу з поточним
DateTime date = new DateTime(2021, 01, 01);
TimeSpan difference = DateTime.Now - date;
if (difference.Days > 30)
{
    Console.WriteLine("Дату вже більше місяця тому.");
}
```

Вихід:

```
Дату вже більше місяця тому.
```

## Дивись також

- [Клас DateTime в C#](https://docs.microsoft.com/en-us/dotnet/api/system.datetime?view=net-5.0)
- [Клас TimeSpan в