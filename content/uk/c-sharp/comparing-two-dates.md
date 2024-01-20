---
title:                "Порівняння двох дат"
html_title:           "Clojure: Порівняння двох дат"
simple_title:         "Порівняння двох дат"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c-sharp/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Що та навіщо?

Порівняння двох дат - це процедура, за допомогою якої ми можемо визначити, яка дата є раніше, пізніше чи обидві дати однакові. Програмісти роблять це, щоб обробляти або маніпулювати информацією, яка базується на датах, в різних сценаріях, такі як сортування відносно дати або обрахунок віків.

## Як зробити:

```C#
DateTime firstDate = new DateTime(2022, 1, 1);
DateTime secondDate = new DateTime(2022, 12, 31);

int result = DateTime.Compare(firstDate, secondDate);

if (result < 0)
    Console.WriteLine("firstDate is earlier than secondDate.");
else if (result == 0)
    Console.WriteLine("firstDate is the same as secondDate");
else
    Console.WriteLine("firstDate is later than secondDate");
```

У вихідних даних: "firstDate is earlier than secondDate."

## Занурення вглиб

Раніше, для порівняння дат, програмісти використовували звичайні оператори порівняння (>, < , ==). Проте, з впровадженням `DateTime.Compare` в .NET Framework, все стало набагато простішим та точнішим.

Альтернативно, можна використовувати альтернативний метод `CompareTo(DateTime value)`, що виконує таку ж функцію.

Детальніше, `DateTime.Compare` повертає значення менше нуля, якщо перша дата раніше другої, нуль якщо дати однакові, або значення більше нуля якщо перша дата пізніше другої.

## Дивіться також:

1. [Object.Equals метод (.NET Framework)](https://docs.microsoft.com/uk-ua/dotnet/api/system.object.equals?view=netframework-4.8): компаратори дат та часу в C#.
2. [DateTime.CompareTo метод (.NET Framework)](https://docs.microsoft.com/uk-ua/dotnet/api/system.datetime.compareto?view=netframework-4.8): альтернативний спосіб порівняння двох дат.
3. [DateTime.Compare метод (.NET Framework)](https://docs.microsoft.com/uk-ua/dotnet/api/system.datetime.compare?view=netframework-4.8): офіційна документація про метод порівняння дат від Microsoft.