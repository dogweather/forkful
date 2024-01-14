---
title:    "C#: З'єднання рядків"
keywords: ["C#"]
---

{{< edit_this_page >}}

## Чому

Можливо, ви чули про потужну функцію з'єднання рядків у C#, але не знаєте, для чого вона потрібна. На щастя, ми з вами сьогодні розвіємо цю загадку і дізнаємось, як та чому треба використовувати з'єднання рядків у своїх програмах.

## Як

З'єднання рядків, також відоме як конкатенація, - це процес об'єднання двох або більше рядків у один. У C# для цього використовується оператор `+` або метод `String.Concat()`.

```C#
// Приклад використання оператора +
string firstName = "Іван";
string lastName = "Петренко";
string fullName = firstName + " " + lastName;

Console.WriteLine(fullName); // Виведе "Іван Петренко"

// Приклад використання методу String.Concat()
string greet = "Привіт, ";
string name = "Оксано";
string message = String.Concat(greet, name, "!");

Console.WriteLine(message); // Виведе "Привіт, Оксано!"
```

Можна також використовувати метод `String.Format()` для конкатенації рядків у зручнішому форматі.

```C#
// Приклад використання методу String.Format()
string firstName = "Іван";
string lastName = "Петренко";
string fullName = String.Format("{0} {1}", firstName, lastName);

Console.WriteLine(fullName); // Виведе "Іван Петренко"
```

## Глибока занурення

Окрім з'єднання рядків, у C# також є можливість форматування великих рядків за допомогою методу `String.Join()`. Він дозволяє об'єднати елементи масиву у один рядок, розділити їх певним символом і навіть вказати форматування для кожного елементу.

```C#
// Приклад використання методу String.Join()
string[] fruits = { "яблуко", "банан", "груша" };
string allFruits = String.Join(", ", fruits);
string formattedFruits = String.Join(", ", fruits.Select(f => $"- {f}"));

Console.WriteLine(allFruits); // Виведе "яблуко, банан, груша"
Console.WriteLine(formattedFruits); /* 
Виведе:
- яблуко
- банан
- груша
*/
```

## Дивіться також

- [Рядкові ряди в C#](https://docs.microsoft.com/uk-ua/dotnet/api/system.string)
- [Оператор конкатенації у C#](https://docs.microsoft.com/uk-ua/dotnet/csharp/language-reference/operators/string-concatenation-operator)
- [Методи форматування рядків у C#](https://docs.microsoft.com/uk-ua/dotnet/standard/base-types/composite-formatting)