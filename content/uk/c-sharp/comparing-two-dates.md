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

## Що і чому?
Порівняння двох дат - це процес визначення взаємного ставлення між двома датами. Програмісти здійснюють такі порівняння для вирішення різноманітних задач, таких як перевірка правильності даних або сортування.

## Як виконати:
```C#
DateTime date1 = new DateTime(2020, 1, 1);
DateTime date2 = new DateTime(2020, 2, 1);

// Порівняння двох дат за допомогою методу CompareTo ()
int result = date1.CompareTo(date2);
// результат = -1, оскільки date1 менше, ніж date2

// Порівняння двох дат за допомогою оператора >
if (date1 > date2)
{
    // виконати певну дію
}

// Порівняння двох дат за допомогою методу Equals ()
bool areEqual = date1.Equals(date2);
// areEqual = false, оскільки date1 і date2 є різними датами
```

## Глибше вдивіться:
Окрім звичайного порівняння за допомогою операторів, існують деякі альтернативи, такі як метод Compare () і клас DateTime.Compare (). Ці альтернативи використовуються для більш складних порівнянь, наприклад, починаючи з рівняння днів тижня або здійснення перевірки на те, чи перебувають дати в одному місяці. Щоб реалізувати такі порівняння, програмісти повинні обережно переглянути документацію та знайти відповідні методи.

## Дивись також:
- [How to Compare Two Dates in C#](https://www.c-sharpcorner.com/article/how-to-compare-two-dates-in-C-Sharp/)
- [DateTime.CompareTo Method](https://docs.microsoft.com/en-us/dotnet/api/system.datetime.compareto?view=netcore-3.1)
- [DateTime.Compare Method](https://docs.microsoft.com/en-us/dotnet/api/system.datetime.compare?view=netcore-3.1)
- [DateTime.Equals Method](https://docs.microsoft.com/en-us/dotnet/api/system.datetime.equals?view=netcore-3.1)