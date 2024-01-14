---
title:    "C#: Вилучення підстрок"
keywords: ["C#"]
---

{{< edit_this_page >}}

[//]: # "Casual C# Programming Blog for Ukrainian Readers"

## Чому

Екстрагування підрядків є важливою частиною програмування в C#. Це дозволяє отримувати частини тексту зі стрічки, що допомагає зробити роботу з текстом більш продуктивною та ефективною.

## Як

Екстрагування підрядків може бути зроблене за допомогою методу `Substring()` в C#. Цей метод приймає два аргументи - індекс початку та довжину підрядка. Давайте розглянемо приклад:
```C#
string text = "Привіт, світ!";
string extractedSubstring = text.Substring(7, 5);
Console.WriteLine(extractedSubstring);
```

Цей код виведе `світ`, оскільки ми починаємо екстрагувати з позиції 7 (починаючи з 0) та беремо 5 символів. Ми також можемо використовувати властивість `Length` у стрічки, щоб отримати кількість символів:
```C#
string text = "Це оригінальний текст.";
string extractedSubstring = text.Substring(4, text.Length - 9);
Console.WriteLine(extractedSubstring);
```

Цей код виведе `оригінальний текст`, оскільки ми починаємо з позиції 4 та беремо довжину стрічки (`text.Length`) та віднімаємо 9 символів, щоб уникнути витягування зайвих символів вкінці.

## Глибока аналітика

Метод `Substring()` також може бути використаний для екстрагування частини підрядка з довільної позиції до кінця стрічки. Для цього ми можемо не задавати другий аргумент (довжину підрядка) та використовувати лише перший аргумент (позицію початку):
```C#
string text = "Це оригінальний текст.";
string extractedSubstring = text.Substring(11);
Console.WriteLine(extractedSubstring);
```

Цей код виведе `текст.`, оскільки ми починаємо з позиції 11 та беремо кінець стрічки. Також, якщо ми не задамо жодного аргументу, то метод `Substring()` поверне всю стрічку.

## Дивись також

- [Документація C# для методу `Substring()`](https://docs.microsoft.com/uk-ua/dotnet/api/system.string.substring)
- [Робота зі стрічками в C# саме по-українськи](https://csharp.net.ua/string/)
- [Екстрагування символів у C#](http://www.csharpstar.com/csharp-working-with-characters/#Extracting_characters)