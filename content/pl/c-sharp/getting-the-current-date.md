---
title:    "C#: Uzyskiwanie aktualnej daty"
keywords: ["C#"]
---

{{< edit_this_page >}}

## Dlaczego

Jeśli programujesz w języku C#, na pewno spotkałeś się z potrzebą uzyskania aktualnej daty. Może być to wykorzystane do różnych celów, np. w celu wyświetlenia daty na stronie internetowej, zapisania jej do bazy danych lub użycia w logowaniu aplikacji. W tym artykule dowiesz się w jaki sposób uzyskać aktualną datę w swoim kodzie.

## Jak to zrobić

Uzyskanie aktualnej daty w języku C# jest bardzo proste. Wystarczy użyć klasy `DateTime` z przestrzeni nazw `System` i wywołać metodę `Now()`. Poniżej znajduje się przykładowy kod, który pokazuje jak uzyskać aktualną datę i wypisać ją w konsoli:

```C#
using System;

namespace CurrentDate
{
    class Program
    {
        static void Main(string[] args)
        {
            DateTime currentDate = DateTime.Now;
            Console.WriteLine(currentDate);

            // Output: 4/13/2021 1:30:24 PM
        }
    }
}
```

W powyższym przykładzie wywołaliśmy metodę `Now()` na obiekcie klasy `DateTime`, który nazwaliśmy `currentDate`. Następnie wypisaliśmy tę datę za pomocą metody `WriteLine()` z przestrzeni nazw `System.Console`. Wynik wyświetlił się w następującym formacie: `miesiąc/dzień/rok godzina:minuta:sekunda AM/PM`.

## Pełna analiza

W rzeczywistości, metoda `Now()` używa również `DateTimeKind` (informacja o strefie czasowej) do określenia aktualnej daty. Jeśli chcesz uzyskać pełną analizę, jak wygląda proces uzyskiwania aktualnej daty w języku C#, zalecamy przeczytanie artykułu "Understanding DateTime in .NET" (https://www.c-sharpcorner.com/article/understanding-datetime-in-net/).

## Zobacz także

- Dokumentacja klasy DateTime w języku C#: https://docs.microsoft.com/pl-pl/dotnet/api/system.datetime?view=net-5.0
- Artykuł "7 sposobów na pracę z datami i godzinami w C#": https://www.syncfusion.com/blogs/post/7-ways-of-working-with-dates-and-times-in-c.aspx