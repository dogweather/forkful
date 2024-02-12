---
title:                "Znalezienie długości ciągu znaków"
aliases: - /pl/c-sharp/finding-the-length-of-a-string.md
date:                  2024-01-20T17:47:09.034892-07:00
model:                 gpt-4-1106-preview
simple_title:         "Znalezienie długości ciągu znaków"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (Co i dlaczego?)
Długość ciągu to po prostu liczba znaków w tekście. Programiści liczą znaki, by zarządzać danymi - na przykład sprawdzając hasła czy ograniczając wprowadzany tekst.

## How to (Jak to zrobić):
W C# masz właściwość `Length` do liczenia znaków:

```C#
string przykladowyTekst = "Witaj świecie!";
int dlugosc = przykladowyTekst.Length;
Console.WriteLine("Długość stringa to: " + dlugosc);
```

Wyjście:

```
Długość stringa to: 14
```

## Deep Dive (W głębi tematu):
W C# `.Length` zwraca `int` reprezentujący liczbę `char` w stringu. Jest to szybka operacja, bo długość ciągu jest przechowywana wewnątrz obiektu `string`. W starszych wersjach języków, jak C, liczenie długości było bardziej skomplikowane, bo wymagało iteracji aż do napotkania znaku końca ciągu. Alternatywą dla `.Length` może być rozszerzenie LINQ, ale to zwykle mniej wydajne. Przy implementacji pamiętaj, że `string` w C# jest niezmienny i kodowanie Unicode może sprawiać, że nie zawsze licznik znaków odpowiada liczbie „widzialnych” symboli (na przykład ze względu na diakrytyki).

## See Also (Zobacz także):
- [Dokumentacja Microsoft dotycząca właściwości Length](https://docs.microsoft.com/en-us/dotnet/api/system.string.length)
- [Rozwinięcie tematu Unicode w C#](https://docs.microsoft.com/en-us/dotnet/standard/base-types/character-encoding)
- [Wprowadzenie do LINQ w C#](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/concepts/linq/)
