---
title:                "Korzystanie z wyrażeń regularnych"
html_title:           "Arduino: Korzystanie z wyrażeń regularnych"
simple_title:         "Korzystanie z wyrażeń regularnych"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?

Wyrażenia regularne (Regex) to mocne narzędzie do przeszukiwania i manipulacji tekstem. Programiści używają ich do sprawdzania poprawności danych, przeszukiwania tekstu, przetwarzania ciągów i do wielu innych zadań.

## Jak to zrobić

Użyj klasy `Regex` z biblioteki `.NET`. Możesz utworzyć nowy obiekt `Regex` z wyrażeniem regularnym jako argumentem. Poniżej znajduje się przykład:

```C#
using System.Text.RegularExpressions;

string testInput = "Hello, my name is Adam and my email is adam@example.com";
Regex regex = new Regex(@"\b[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Z|a-z]{2,7}\b");
Match match = regex.Match(testInput);

if (match.Success)
{
    Console.WriteLine($"Znaleziono adres e-mail: {match.Value}");
}
```

To wyrażenie regularne spowoduje wyświetlenie `Znaleziono adres e-mail: adam@example.com` w konsoli.

## Zanurz się głębiej

Wyrażenia regularne mają długą historię. Pierwsze rządy komputerów, jak Edsac, używały ich do przeszukiwania i manipulacji tekstami. W C#, Microsoft oferuje mocną implementację wyrażeń regularnych. 

Jest wiele alternatyw dla wyrażeń regularnych w programowaniu, takich jak metody szukania ciągu czy metody kontekstowe, ale żadna z nich nie jest równie wszechstronna jak wyrażenia regularne.

Dotyczące szczegółów implementacji, biblioteka .NET używa automatów skończonych do kompilacji i wykonywania wyrażeń regularnych, co daje bardzo dobre rezultaty.

## Zobacz także

Dla dalszego zrozumienia, zajrzyj do tych źródeł:

- Dokumentacja Microsoft na temat wyrażeń regularnych w C#: <br>https://docs.microsoft.com/pl-pl/dotnet/standard/base-types/regular-expressions
- Online Regex tester i debugger: <br>https://regex101.com/
- Kursy o wyrażeniach regularnych na Udemy: <br>https://www.udemy.com/topic/regular-expression/