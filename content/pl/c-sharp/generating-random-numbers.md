---
title:                "Generowanie liczb losowych"
html_title:           "Gleam: Generowanie liczb losowych"
simple_title:         "Generowanie liczb losowych"
programming_language: "C#"
category:             "C#"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/generating-random-numbers.md"
---

{{< edit_this_page >}}

Tituł: Generowanie liczb losowych w języku C#

## Co i Dlaczego? (What & Why?)

Generowanie liczb losowych to proces tworzenia ciągu liczb, których wystąpienie jest nieprzewidywalne. Programiści to robią, aby dodawać element losowości do swoich aplikacji, czy to w grach, symulacjach czy bezpieczeństwie danych.

## Jak to zrobić: (How to:)

W C# generowanie liczb losowych jest proste, dzięki wbudowanej klasie `Random`. Poniżej znajduje się prosty przykład.

```C#
using System;

class Program
{
    static void Main()
    {
        Random randNumberGenerator = new Random();
        int randomNum = randNumberGenerator.Next(1, 100);  // Generuje liczbę losową między 1 a 100
        Console.WriteLine(randomNum);
    }
}
```
Kiedy uruchomisz ten kod, dostaniesz na wyjściu losową liczbę między 1 a 100. Wypróbuj sam!

## Dogłębna analiza (Deep Dive)

**Kontekst historyczny:** Wcześniej generowanie liczb losowych było trudniejsze i wymagało skomplikowanych algorytmów, ale z czasem języki programowania ułatwiły ten proces.

**Alternatywy:** Możesz użyć różnych metod do generowania liczb losowych w C#, na przykład:
`RNGCryptoServiceProvider` daje bardziej losowe liczby, ale jest wolniejszy od klasy `Random`.

**Szczegóły implementacji:** Klasa `Random` w C# korzysta z algorytmu pseudolosowego, co oznacza, że liczby wydają się losowe, ale powtarzając program z tym samym ziarnem losowości (`seed`), wygenerowane liczby będą takie same.

## Zobacz też (See Also):

- https://docs.microsoft.com/pl-pl/dotnet/api/system.random?view=net-5.0: dokładne informacje na temat klasy `Random` w .NET 5.0 
- https://en.wikipedia.org/wiki/Pseudorandom_number_generator: Więcej informacji na temat generatorów liczb pseudolosowych
- https://docs.microsoft.com/pl-pl/dotnet/api/system.security.cryptography.rngcryptoserviceprovider?view=net-5.0: Dokumentacja `RNGCryptoServiceProvider` w .NET 5.0