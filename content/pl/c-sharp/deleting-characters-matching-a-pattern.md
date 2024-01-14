---
title:    "C#: Usuwanie znaków pasujących do wzorca"
keywords: ["C#"]
---

{{< edit_this_page >}}

## Dlaczego

Usuwanie znaków pasujących do wzorca może być bardzo przydatne w wielu różnych sytuacjach, na przykład podczas przetwarzania danych wejściowych lub weryfikacji danych użytkownika. Może również pomóc poprawić wydajność programu, szczególnie jeśli musimy przetworzyć duże ilości danych.

## Jak To Zrobić

Aby usunąć znaki pasujące do wzorca, użyjemy metody `Regex.Replace()` dostępnej w języku C#. Ta metoda przyjmuje trzy parametry: łańcuch wejściowy, wzorzec i łańcuch zastępczy. Wzorzec musi być wyrażeniem regularnym, które określa, które znaki mają zostać usunięte. Poniżej znajduje się przykładowy kod, który usuwa wszystkie cyfry występujące w łańcuchu wejściowym:

```C#
string input = "abc123def456ghi789";
string pattern = @"\d"; // wzorzec dla cyfr
string replacement = ""; // pusty łańcuch zastępczy
string output = Regex.Replace(input, pattern, replacement);
Console.WriteLine(output); // wynik: abcdefghi
```

Możesz również użyć wyrażenia regularnego w celu usunięcia konkretnych znaków, na przykład wszystkich małych liter lub interpunkcji. Poniżej znajduje się przykład usuwania kropek, przecinków i średników z łańcucha wejściowego:

```C#
string input = "To jest przykład wyrażenia, które. usuwa; znaki. interpunkcyjne";
string pattern = @"[,.;]"; // wzorzec dla kropek, przecinków i średników
string replacement = ""; // pusty łańcuch zastępczy
string output = Regex.Replace(input, pattern, replacement);
Console.WriteLine(output); // wynik: To jest przykład wyrażenia które usuwa znaki interpunkcyjne
```

Możemy również użyć wyrażenia regularnego, aby usunąć znaki na podstawie określonych warunków, na przykład wszystkie cyfry mniejsze niż 5 lub tylko duże litery. Istnieje wiele różnych możliwości, dlatego zachęcamy do eksperymentów i dostosowywania wyrażeń regularnych do swoich potrzeb.

## Deep Dive

Głównym narzędziem wykorzystywanym do usuwania znaków pasujących do wzorca jest klasa `Regex` dostępna w przestrzeni nazw `System.Text.RegularExpressions`. Pozwala ona na stosowanie złożonych, ale skutecznych wyrażeń regularnych, które mogą zostać dostosowane do różnych zastosowań. Możesz dowiedzieć się więcej o wyrażeniach regularnych i jak je wykorzystywać w języku C# na oficjalnej stronie dokumentacji Microsoft.

See Also

- Przewodnik po wyrażeniach regularnych w języku C# (https://docs.microsoft.com/pl-pl/dotnet/standard/base-types/regular-expression-language-quick-reference)
- Wprowadzenie do przetwarzania łańcuchów tekstowych z wykorzystaniem wyrażeń regularnych w C# (https://docs.microsoft.com/pl-pl/dotnet/standard/base-types/regular-expressions)
- Praktyczne przykłady zastosowania wyrażeń regularnych w języku C# (https://www.c-sharpcorner.com/article/all-about-regular-expressions-in-c-sharp/)