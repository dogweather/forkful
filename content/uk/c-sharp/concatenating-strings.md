---
title:                "Конкатенація рядків"
html_title:           "PHP: Конкатенація рядків"
simple_title:         "Конкатенація рядків"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c-sharp/concatenating-strings.md"
---

{{< edit_this_page >}}

## Що і чому?

Конкатенація рядків - це спосіб об'єднання двох або більше рядків. Програмісти роблять це, щоб створити повніші рядки чи динамічно змінювати вміст рядка.

## Як зробити:

Нижче наведений приклад коду, який демонструє роботу з конкатенацією рядків у C#.

```C#
string firstName = "Олександр";
string lastName = "Іванов";
string fullName = firstName + " " + lastName;

Console.WriteLine(fullName);
```

В результаті ви отримаєте виведення:

```
Олександр Іванов
```

## Пірнемо глибше:

Історично багато мов програмування включають функцію конкатенації рядків. З часом розробники почали використовувати більш ефективні способи об'єднання рядків, такі як StringBuilder в C#, який забезпечує кращу продуктивність при об'єднанні великої кількості рядків.

## Дивіться також:

1. [Документація Microsoft з конкатенації рядків](https://docs.microsoft.com/uk-ua/dotnet/csharp/programming-guide/strings/how-to-concatenate-multiple-strings)
2. [StringBuilder в C#](https://docs.microsoft.com/uk-ua/dotnet/api/system.text.stringbuilder?view=net-5.0)
3. [Метод Join() в C#](https://docs.microsoft.com/uk-ua/dotnet/api/system.string.join?view=net-5.0)