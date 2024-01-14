---
title:                "C#: Zmiana ciągu znaków na duże litery"
simple_title:         "Zmiana ciągu znaków na duże litery"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Kapitalizacja to podstawowa czynność w programowaniu, która jest niezbędna do wielu zadań. Gdy chcemy, aby nasz program był czytelny dla użytkownika lub wyświetlany w odpowiedni sposób, często musimy skorzystać z kapitalizacji. W tym blogu dowiesz się, jak w prosty sposób zastosować tę funkcję w języku C#.

## Jak To Zrobić

Kapitalizacja w C# odbywa się za pomocą metody `ToUpper`, która zmienia wszystkie litery w ciągu znaków na wielkie. Przykładowy kod wyglądałby następująco:

```C#
string name = "jan kowalski";

Console.WriteLine(name.ToUpper());
```

W powyższym przykładzie, wartość zmiennej `name` zostanie zmieniona na "JAN KOWALSKI". Możemy również zastosować funkcję `ToLower`, która zmieni wszystkie litery na małe.

Dodatkowo, możemy również wykorzystać metodę `ToTitleCase` do kapitalizacji tylko pierwszej litery w każdym wyrazie. Przykładowy kod wyglądałby w ten sposób:

```C#
string sentence = "cześć, jak się masz?";

// Dodajemy referencję do przestrzeni nazw System.Globalization
string capitalizedSentence = System.Globalization.CultureInfo.CurrentCulture.TextInfo.ToTitleCase(sentence);

Console.WriteLine(capitalizedSentence);
```

W tym przypadku, wartość zmiennej `capitalizedSentence` zostanie zmieniona na "Cześć, Jak Się Masz?".

## Deep Dive

Metody `ToUpper` i `ToLower` działają poprawnie tylko w przypadku standardowych znaków ASCII. Jeśli jednak potrzebujemy kapitalizować znaki spoza tego zakresu, musimy użyć funkcji `ToUpperInvariant` lub `ToLowerInvariant`. Te metody są bardziej elastyczne i działają dla wszystkich znaków, niezależnie od ich kodowania.

Dodatkowo, warto również zwrócić uwagę na wydajność kodu. W przypadku dużych ilości danych do kapitalizacji, zaleca się używanie StringBuilder, aby uniknąć tworzenia zbędnych obiektów typu string.

## Zobacz również

- [Metoda ToUpper w dokumentacji języka C#](https://docs.microsoft.com/pl-pl/dotnet/api/system.string.toupper)
- [Przestrzeń nazw System.Globalization w dokumentacji języka C#](https://docs.microsoft.com/pl-pl/dotnet/api/system.globalization?view=netframework-4.8)
- [Optymalizacja kodu w języku C#](https://docs.microsoft.com/pl-pl/dotnet/csharp/programming-guide/concepts/code-quality/optimize-your-code)

Dziękujemy za lekturę tego bloga. Mamy nadzieję, że dzięki niemu dowiedziałeś się, jak w prosty sposób zastosować kapitalizację do ciągu znaków w języku C#.