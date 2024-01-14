---
title:    "C#: Отримання поточної дати"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/uk/c-sharp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Чому

Отримання поточної дати є важливою складовою при програмуванні, оскільки це допомагає відстежувати час і дату подій в програмі. Це також може бути корисно при роботі з даними, які мають обмеження за часом, наприклад, приймання платежів чи запланованих завдань.

## Як

Для отримання поточної дати використовується клас ```DateTime``` у мові програмування C#. Цей клас надає різноманітні методи для роботи з датами і часом. Один з таких методів - ```DateTime.Now```, який повертає поточну дату і час у форматі ```DateTime```.

```c#
DateTime currentDate = DateTime.Now; // Отримання поточної дати
Console.WriteLine(currentDate); // Вивід на екран поточної дати
```

Результат виконання цього коду буде наступним:

``` 
5/15/2021 10:30:45 AM // Дата та час, коли код був запущений
```

Щоб отримати поточну дату у іншому форматі, можна використовувати метод ```DateTime.ToString()``` з параметром, який визначає необхідний формат виведення:

```c#
DateTime currentDate = DateTime.Now;
Console.WriteLine(currentDate.ToString("dd/MM/yyyy")); // Вивід на екран поточної дати у форматі dd/MM/yyyy
```

Результат буде наступним:

```
15/05/2021 // Дата у форматі dd/MM/yyyy
```

## Детальніше

Клас ```DateTime``` містить також інші корисні методи для роботи з датами, наприклад, ```DateTime.Parse()``` для перетворення рядка у ```DateTime``` або ```DateTime.AddDays()``` для додавання днів до поточної дати.

Також було введено нові класи і методи для роботи з датами в C# 8.0, такі як ```DateTimeOffset``` і ```DateTime.TryParseExact()```, які роблять роботу з датами ще більш зручною та ефективною.

## Дивіться також

- [DateTime - C# Reference](https://docs.microsoft.com/en-us/dotnet/api/system.datetime?view=net-5.0)
- [DateTime Tutorial - w3schools](https://www.w3schools.com/cs/cs_datetime.asp)
- [C# 8.0: Beyond #148B](https://docs.microsoft.com/en-us/archive/msdn-magazine/2019/september/csharp-8-0-beyond-8e7f224f-65a9-4fd1-8d9a-8e61239c74d0)