---
title:                "C#: Konwertowanie ciągu znaków na małe litery"
simple_title:         "Konwertowanie ciągu znaków na małe litery"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Dlaczego

Konwertowanie napisów na małe litery jest częstym zadaniem w programowaniu. Jest to przydatne do porównywania i sortowania tekstów w jednolitym formacie. W tym artykule dowiesz się, jak to zrobić w języku C#.

## Jak

Konwertowanie napisu na małe litery w języku C# jest bardzo proste. Wystarczy użyć funkcji `ToLower()` na obiekcie typu `string`.

```C#
// przykładowy napis
string napis = "PROGRAMOWANIE";
// konwertowanie na małe litery
string napisMaleLitery = napis.ToLower();
// wyświetlenie wyniku
Console.WriteLine(napisMaleLitery); // output: "programowanie"
```

Możemy również użyć funkcji `ToLowerInvariant()`, która zawsze zwraca takie same litery niezależnie od ustawień regionalnych.

```C#
// przykładowy napis
string napis = "PROGRAMOWANIE";
// konwertowanie na małe litery
string napisMaleLitery = napis.ToLowerInvariant();
// wyświetlenie wyniku
Console.WriteLine(napisMaleLitery); // output: "programowanie"
```

Inną opcją jest użycie metody `CultureInfo` do ustalenia określonych ustawień regionalnych dla naszego napisu.

```C#
// przykładowy napis
string napis = "PROGRAMOWANIE";
// ustalenie ustawień regionalnych
CultureInfo culture = new CultureInfo("pl-PL");
// konwertowanie na małe litery
string napisMaleLitery = napis.ToLower(culture);
// wyświetlenie wyniku
Console.WriteLine(napisMaleLitery); // output: "programowanie"
```

## Deep Dive

Podczas konwertowania napisu na małe litery warto mieć na uwadze kilka rzeczy. Po pierwsze, `ToLower()` nie zmienia oryginalnego napisu, tylko zwraca nowy napis z małymi literami. Jeśli chcesz zmienić oryginalny napis, musisz przypisać zwrócony napis do zmiennej.

```C#
// przykładowy napis
string napis = "PROGRAMOWANIE";
// konwertowanie na małe litery
string napisMaleLitery = napis.ToLower();
// zmiana oryginalnego napisu
napis = napisMaleLitery;
```

Warto również pamiętać o tym, że funkcja `ToLower()` nie konwertuje polskich znaków na małe litery. Jeśli chcemy uzyskać napis ze wszystkimi literami małymi, musimy wykorzystać inną metodę, np. `ToLowerInvariant()`, która uwzględnia wszystkie znaki.

## Zobacz również

- [Dokumentacja języka C# - Metoda ToLower()](https://docs.microsoft.com/pl-pl/dotnet/api/system.string.tolower?view=net-5.0)
- [Dokumentacja języka C# - Metoda ToLowerInvariant()](https://docs.microsoft.com/en-us/dotnet/api/system.string.tolowerinvariant?view=net-5.0)
- [Dokumentacja języka C# - klasa CultureInfo](https://docs.microsoft.com/pl-pl/dotnet/api/system.globalization.cultureinfo?view=net-5.0)