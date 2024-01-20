---
title:                "Знаходження довжини рядка"
html_title:           "Arduino: Знаходження довжини рядка"
simple_title:         "Знаходження довжини рядка"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c-sharp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Що і чому?

Пошук довжини рядка - це процес визначення кількості символів у даному рядку. Програмісти роблять це, щоб керувати процесами обробки даних та валідації.

## Як це зробити:

В C# для підрахунку символів у рядку використовується властивість `Length`:

```C#
string str = "Привіт, світ!";
int len = str.Length;
Console.WriteLine(len);
```
В результаті виводу команди ми отримаємо "13", оскільки рядок "Привіт, світ!" містить 13 символів.

## Поглиблений розгляд:

Історично властивість `Length` була визначена у зв'язку з потребами в обробці рядків даних. Ця особливість присутня в багатьох мовах програмування, не лише в C#.

Альтернативний спосіб визначення довжини рядка в C# - це використовувати метод `.Count()` з бібліотеки System.Linq:

```C#
string str = "Привіт, світ!";
int len = str.Count();
Console.WriteLine(len);
```
Але будьте обережні, цей метод повертає правильну довжину лише для рядків без спеціальних символів, таких як `\r\n`. В іншому випадку він зарахує їх за два символи.

Використовуючи `Length` для рядків в C#, насправді ми звертаємось до довжини масиву `char`, що є базовим представленням рядка в пам'яті. 

## Також дивіться:

1. [Microsoft Docs: рядкові властивості](https://docs.microsoft.com/uk-ua/dotnet/api/system.string.length?view=net-5.0)
2. [StackOverflow: розуміння властивості Length](https://stackoverflow.com/questions/1580786/length-of-string-in-c-sharp)
3. [CsharpStar: робота з рядками в C#](http://www.csharpstar.com/csharp-string-length-property/)