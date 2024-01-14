---
title:                "C#: Konwertowanie ciągu znaków na małe litery."
programming_language: "C#"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Dlaczego

Pisanie kodu w języku C# jest nie tylko zabawne, ale także przydatne w codziennych zastosowaniach. Jednym z przydatnych funkcji w tym języku jest konwertowanie ciągów znaków na małe litery. W tym artykule dowiesz się, dlaczego jest to przydatne i jak tego dokonać.

## Jak to zrobić

```C#
string name = "JANUSZ";
Console.WriteLine(name.ToLower());
```

Output: janusz

Proces konwersji ciągu znaków na małe litery jest bardzo prosty w języku C#. W powyższym przykładzie wykorzystujemy metodę `ToLower()`, która konwertuje wszystkie litery w podanym ciągu na ich małe odpowiedniki. Następnie, wyświetlamy zmienną `name` z zastosowaniem tej metody, co daje nam oczekiwany wynik.

## Wnikliwa analiza

Konwersja ciągu znaków na małe litery może być szczególnie przydatna w obróbce tekstu lub wykonywaniu zapytań do bazy danych, gdzie różnica w wielkości liter może mieć znaczenie. Warto również pamiętać, że metoda `ToLower()` uwzględnia różnice między językami, co może być istotne w przypadku międzynarodowych aplikacji.

## Zobacz również
- [Dokumentacja Microsoft dla metody ToLower()](https://docs.microsoft.com/pl-pl/dotnet/api/system.string.tolower)
- [Przetwarzanie ciągów znaków w C#](https://docs.microsoft.com/pl-pl/dotnet/csharp/programming-guide/strings/)