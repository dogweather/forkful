---
title:                "C#: Używanie wyrażeń regularnych"
simple_title:         "Używanie wyrażeń regularnych"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Dlaczego warto używać wyrażeń regularnych?

Regular expressions, also known as regex, are an incredibly powerful tool for pattern matching and string manipulation in programming. They allow for efficient and precise searching and replacing of text, making them a valuable resource for any developer.

## Jak używać wyrażeń regularnych w C#?

Aby używać wyrażeń regularnych w języku C#, musimy najpierw skorzystać z klasy Regex. W przykładzie poniżej wykorzystamy regex do znalezienia wszystkich liczb w ciągu znaków.

```C#
using System;
using System.Text.RegularExpressions;

class Program
{
    static void Main()
    {
        // Utwórz nowy wyrażenie regularne
        Regex regex = new Regex("\\d+");

        // Przykładowy tekst do przeszukania
        string text = "Dzisiaj jest 15 sierpnia, jutro będzie 16.";

        // Użyj metody Match do znalezienia dopasowań
        MatchCollection matches = regex.Matches(text);

        // Wyświetl wszystkie dopasowania
        foreach (Match match in matches)
        {
            Console.WriteLine(match.Value);
        }
    }
}
```

Output:
```
15
16
```

## Głębszy wgląd w użycie wyrażeń regularnych

Wyrażenia regularne oferują wiele funkcjonalności, takich jak grupowanie, alternatywy, kwantyfikatory i wiele innych. Mogą być również wykorzystywane do walidacji danych, formatowania tekstów czy nawet tworzenia skomplikowanych wzorców wyszukiwania.

Jednym z wyrażeń regularnych, które warto poznać, jest `\w+`, które znajduje wszystkie słowa w tekście. Możemy również wykorzystać wyrażenie `\\s` do zbadania białych znaków lub `.` do znalezienia dowolnego znaku.

## Zobacz także

- [Dokumentacja C# - Wyrażenia regularne](https://docs.microsoft.com/pl-pl/dotnet/standard/base-types/regular-expression-language-quick-reference)
- [10 przykładów wykorzystania wyrażeń regularnych w C#](https://www.c-sharpcorner.com/UploadFile/cd7c2e/regular-expression-in-C-Sharp/)