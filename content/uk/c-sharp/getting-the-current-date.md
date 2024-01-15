---
title:                "Отримання поточної дати."
html_title:           "C#: Отримання поточної дати."
simple_title:         "Отримання поточної дати."
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c-sharp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Ось чому
Знаходження поточної дати є незамінною функцією в будь-якій розробці програмного забезпечення. Це дозволяє користувачам бачити оновлену інформацію, включати динамічні функції та стежити за часом.

## Як це зробити
```C#
// Використовуємо клас DateTime для отримання поточної дати і часу
DateTime currentDate = DateTime.Now;
// Виводимо результат у зручному форматі
Console.WriteLine("Поточна дата: " + currentDate.ToShortDateString());
```

Вихід: Поточна дата: 03/03/2021

## Поглиблене дослідження
Крім отримання поточної дати і часу, клас DateTime також надає можливість працювати з датами у різних форматах, порівнювати та обчислювати їх різницю.

Наприклад, можна використовувати методи Add і Subtract для додавання або віднімання певної кількості років, місяців, днів, годин, хвилин або секунд від поточної дати. Також можна порівнювати дати за допомогою методів Compare і Equals, а також обчислювати різницю між ними за допомогою методів Substract і TotalDays.

## Дивись також
- [Клас DateTime у документації Microsoft](https://docs.microsoft.com/dotnet/api/system.datetime)
- [Стаття про роботу з датами та часом в C#](https://docs.microsoft.com/dotnet/standard/base-types/working-with-dates-and-times)