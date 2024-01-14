---
title:                "C#: Порівняння двох дат"
simple_title:         "Порівняння двох дат"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c-sharp/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Чому

Порівняння дат - це важлива задача в багатьох програмах, особливо тих, що працюють зі специфічною інформацією, яка пов'язана зі строками. Такі програми можуть бути, наприклад, календарі, особисті органайзери, системи бронювання та інші. Порівнюючи дати, ми можемо перевірити, чи вони співпадають або одна дата наступає раніше за іншу.

## Як

Для порівняння двох дат в C# ми можемо використати вбудований клас `DateTime` та його методи `Compare`, `Equals` та `CompareTo`. Нижче наведено приклад коду з використанням цих методів:

```C#
// Створюємо дві дати для порівняння
DateTime date1 = new DateTime(2020, 10, 5); 
DateTime date2 = new DateTime(2020, 8, 30);

// Порівнюємо дати за допомогою методу Compare
int result = DateTime.Compare(date1, date2);

if (result < 0)
{
    Console.WriteLine("Дата 1 передує Даті 2");
}
else if (result == 0)
{
    Console.WriteLine("Обидві дати співпадають");
}
else
{
    Console.WriteLine("Дата 2 передує Даті 1");
}

// Використовуємо метод Equals для перевірки чи дати співпадають
if (date1.Equals(date2))
{
    Console.WriteLine("Дата 1 і Дата 2 співпадають");
}
else
{
    Console.WriteLine("Дата 1 і Дата 2 НЕ співпадають");
}

// Використовуємо метод CompareTo для порівняння дат за допомогою оператора <, > та ==
if (date1.CompareTo(date2) < 0)
{
    Console.WriteLine("Дата 1 передує Даті 2");
}
else if (date1.CompareTo(date2) == 0)
{
    Console.WriteLine("Дати співпадають");
}
else
{
    Console.WriteLine("Дата 2 передує Даті 1");
}

```

Результат виконання програми буде наступним:

```
Дата 2 передує Даті 1
Дата 1 і Дата 2 НЕ співпадають
Дата 2 передує Даті 1
```

## Глибокий погляд

Крім методів порівняння, клас DateTime має також інші корисні методи для роботи з датами, такі як `Add`, `Subtract`, `ToString` та інші. Клас також містить властивості для доступу до окремих компонентів дати, наприклад, рік, місяць, день та інші. Більш детальну інформацію про клас DateTime та його методи можна знайти на офіційній документації Microsoft.

## Дивись також

- [Офіційна документація Microsoft про клас DateTime](https://docs.microsoft.com/uk-ua/dotnet/api/system.datetime?view=netcore-3.1)
- [Порівняння дат в C#](https://www.tutorialspoint.com/csharp/csharp