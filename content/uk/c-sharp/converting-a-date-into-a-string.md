---
title:                "C#: Перетворення дати у рядок."
programming_language: "C#"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c-sharp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Чому

Один з найпоширеніших завдань для програмістів - це робота з датами. Однак, іноді нам потрібно перетворити дату на рядок для збереження або виведення її в форматі, зрозумілому людям. Саме тому в цій статті ми розглянемо, як перетворити дату у рядок в C#.

## Як це зробити

Найпростіший спосіб конвертувати дату в рядок - це використовувати метод `ToString()` з класу `DateTime`. Нижче наведений приклад коду, де ми перетворюємо дату у рядок у форматі MM/dd/yyyy.

```C#
DateTime date = new DateTime(2020, 12, 1);
string dateString = date.ToString("MM/dd/yyyy");
Console.WriteLine(dateString); // Виведе 12/01/2020
```

Однак, метод `ToString()` може приймати різні параметри форматування, що дозволяє нам застосовувати більш розширені можливості. Наприклад, додавши літеру "d" до кінця формату, ми можемо вивести лише день місяця:

```C#
DateTime date = new DateTime(2020, 12, 1);
string dateString = date.ToString("d");
Console.WriteLine(dateString); // Виведе 1
```

Ви також можете використовувати метод `ToString()` зі строковою змінною, яка містить формат дати, аби перетворити дату у рядок:

```C#
DateTime date = new DateTime(2020, 12, 1);
string format = "dd/MM/yyyy";
string dateString = date.ToString(format);
Console.WriteLine(dateString); // Виведе 01/12/2020
```

## Глибоке занурення

Якщо ви зацікавлені в більш глибокому вивченні перетворення дати в рядок в C#, вам варто ознайомитися з методом `ToString()` класу `DateTime` ближче. Цей метод має багато різноманітних параметрів форматування, які дозволяють вам сконструювати рядок з датою в будь-якому форматі, який вам знадобиться. Також варто зазначити, що є інші способи перетворення дати в рядок, такі як виконання ручної обробки за допомогою методів класу `DateTime`, але метод `ToString()` є найпростішим і найзручнішим.

## Дивіться також

- [DateTime.ToString() Method (Microsoft Docs)](https://docs.microsoft.com/en-us/dotnet/api/system.datetime.tostring)
- [Custom DateTime Format Strings (Microsoft Docs)](https://docs.microsoft.com/en-us/dotnet/standard/base-types/custom-date-and-time-format-strings)
- [How to convert DateTime to string in C# (Stack Overflow)](https://stackoverflow.com/questions/9395128/how-to-convert-datetime-to-string-in-c-sharp)