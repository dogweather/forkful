---
title:    "C#: Usuwanie znaków pasujących do wzorca"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Dlaczego

Często w programowaniu spotykamy się z sytuacją, w której musimy usunąć pewne znaki z tekstu, aby uzyskać oczekiwany wynik. Może to być na przykład usunięcie wszystkich spacji z adresu URL lub usunięcie znaków specjalnych z ciągu tekstowego. Istnieje wiele różnych sposobów na realizację tego zadania, a w tym artykule skupimy się na usuwaniu znaków pasujących do wzorca za pomocą języka C#.

## Jak to zrobić

Aby usunąć znaki pasujące do wzorca, wykorzystujemy metody dostępne w klasie Regex z przestrzeni nazw System.Text.RegularExpressions. Pozwala nam to na wykorzystanie wyrażeń regularnych, które są potężnym narzędziem do manipulacji tekstem. Przykładowy kod w języku C# wyglądałby następująco:

```C#
string sentence = "Tylko 17 dni do wakacji!";
Regex regex = new Regex("[0-9]"); // usuwamy wszystkie cyfry
string output = regex.Replace(sentence, string.Empty);
Console.WriteLine(output);

// Output: Tylko dni do wakacji!
```

W tym przykładzie użyliśmy wyrażenia regularnego "[0-9]", które znajduje wszystkie cyfry w podanym tekście i zastępuje je pustym ciągiem, usuwając je tym samym. Możemy również zastosować bardziej złożone wyrażenia regularne, aby usuwać konkretne znaki lub wzorce w tekście.

## Deep Dive

Klasa Regex udostępnia wiele różnych metod, które pozwalają na bardziej zaawansowane manipulowanie tekstem. Na przykład, możemy użyć metody Match(), aby dopasować wyrażenie regularne do tekstu i wyodrębnić tylko część, którą chcemy zachować. Możemy również wykorzystać metody IsMatch() lub Replace() wraz z wyrażeniami regularnymi, aby przeprowadzić bardziej złożone operacje na tekście.

Warto również wspomnieć o specjalnych znakach (tzw. metaznakach) w wyrażeniach regularnych, takich jak "^" oznaczający początek tekstu lub "$" oznaczający koniec tekstu. Używając tych znaków w połączeniu z innymi znakami, możemy precyzyjniej określić, które znaki chcemy usunąć w danym tekście.

## Zobacz również

- Oficjalna dokumentacja języka C#: https://docs.microsoft.com/pl-pl/dotnet/csharp/
- Wprowadzenie do wyrażeń regularnych w C#: https://docs.microsoft.com/pl-pl/dotnet/standard/base-types/regular-expression-language-quick-reference
- Przykłady użycia wyrażeń regularnych: https://regex101.com/