---
title:                "З'єднання рядків"
html_title:           "C#: З'єднання рядків"
simple_title:         "З'єднання рядків"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c-sharp/concatenating-strings.md"
---

{{< edit_this_page >}}

## Чому

Чому хтось хотів би зайнятися злиттям рядків? Процес злиття рядків дуже корисний для створення багатьох різних варіантів повідомлень і їх форматування у програмах на C#.

## Як

```C#
string firstName = "John";
string lastName = "Smith";

// Злиття двох рядків за допомогою оператора +
string fullName = firstName + " " + lastName;
Console.WriteLine(fullName); // Виведе "John Smith"

// Злиття рядків при використанні методу Concat
string fullName2 = string.Concat(firstName, " ", lastName);
Console.WriteLine(fullName2); // Виведе "John Smith"
```

## Глибоке дослідження

Перш ніж глибше вивчити процес злиття рядків, важливо врахувати деякі деталі. Перш за все, строкові змінні в C# є незмінними, тобто не можуть бути модифіковані після того, як були створені. Тому кожного разу, коли ми злиттям створюємо новий рядок, фактично ми створюємо цілком новий об'єкт із змінним значенням. Це може призвести до більшої споживаності пам'яті, тому важливо розуміти, коли і де краще використовувати злиття рядків. Також варто пам'ятати, що оператор "+" в деяких випадках може бути недоцільним, тому рекомендується використовувати метод Concat для більш гнучкого управління рядками.

## Дивись також

- [Рядки у C#](https://docs.microsoft.com/uk-UA/dotnet/csharp/programming-guide/strings/)
- [Рядки та рядкові класи в C#](https://docs.microsoft.com/uk-UA/dotnet/csharp/language-reference/keywords/string)
- [5 порад по роботі з рядками в C#](https://www.c-sharpcorner.com/article/5-tips-to-work-with-strings-in-c-sharp/)