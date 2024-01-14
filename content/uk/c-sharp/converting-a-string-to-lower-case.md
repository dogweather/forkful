---
title:                "C#: Перетворення рядка в нижній регістр"
simple_title:         "Перетворення рядка в нижній регістр"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c-sharp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Чому

У будь-якому проекті з використанням мови програмування C# неохідно мати знання про роботу з різними типами даних, включаючи роботу з рядками. Часто у розробці додатків виникає потреба у перетворенні рядка в нижній регістр. Розглянемо цей процес детальніше.

## Як

Існує кілька способів перетворення рядка на нижній регістр в мові програмування C#. Ми можемо використовувати вбудовані методи, які надає .NET Framework, або написати свій власний алгоритм. Перш ніж розглядати код, потрібно зрозуміти, що таке рядок в C#.

```C#
string myString = "HELLO WORLD";
```

Цей рядок має тип даних `string`, який є набором символів. Для перетворення у нижній регістр ми можемо використовувати метод `ToLower()`, який надає клас `String`.

```C#
string myLowerString = myString.ToLower();
Console.WriteLine(myLowerString);
```

В результаті отримаємо:

```
hello world
```

Також ми можемо використати метод `ToLowerInvariant()`, який ігнорує регіональні налаштування, що може бути корисним при роботі з різними мовами.

```C#
string myInvariantLowerString = myString.ToLowerInvariant();
Console.WriteLine(myInvariantLowerString);
```

Якщо вам потрібно перетворити лише перший символ рядка, ви можете використати метод `ToLower()` в класі `Char`, передавши перший символ рядка.

```C#
string myFirstLetterLowerString = Char.ToLower(myString[0]) + myString.Substring(1);
Console.WriteLine(myFirstLetterLowerString);
```

Цей код дозволить змінити перший символ рядка на нижній регістр, залишаючи решту такою, яка є.

## Докладніше

Використання методів `ToLower()` та `ToLowerInvariant()` може бути корисним при порівнянні рядків. Наприклад, якщо ви порівнюєте два рядки за допомогою методу `Equals()`, то ви можете отримати неочікуваний результат, оскільки вони будуть різницею лише у регістрі символів. Але якщо використати ці методи, то всі символи будуть перетворені в однаковий регістр, що дозволить коректно порівнювати рядки.

## Дивіться також

1. [Документація Microsoft про роботу з рядками в C#](https://docs.microsoft.com/uk-ua/dotnet/csharp/programming-guide/strings/)
2. [Стаття про метод `ToLower()` в C#](https://www.w3schools.com/cs/cs_string_tolower.asp)
3. [Стаття про метод `ToLowerInvariant()` в C#](https://www.tutorialspoint.com/csharp/csharp_to_lowerinvariant.htm)