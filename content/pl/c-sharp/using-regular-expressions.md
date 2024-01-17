---
title:                "Używanie wyrażeń regularnych"
html_title:           "C#: Używanie wyrażeń regularnych"
simple_title:         "Używanie wyrażeń regularnych"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Co & Dlaczego?

Wykorzystanie wyrażeń regularnych jest powszechną praktyką w świecie programowania. Pozwalają one na szybkie i precyzyjne wyszukiwanie oraz manipulację tekstem. Dzięki nim programiści mogą w prosty sposób przetwarzać duże ilości danych, co jest niezwykle przydatne w pracy z tekstowymi plikami czy bazami danych.

## Jak to zrobić:

Wyrażenia regularne w C# są dostępne w przestrzeni nazw ```System.Text.RegularExpressions```. Przykładowe zastosowanie to znajdowanie i zamiana wyrażeń w tekście, na przykład:

```C#
string input = "Dzisiaj jest piękny dzień!";
string output = Regex.Replace(input, "piękny", "wspaniały");
Console.WriteLine(output); // Wspaniały dzień!
```

Innym przydatnym narzędziem jest wykorzystanie wyrażeń regularnych do wyodrębnienia konkretnych informacji z tekstu, na przykład:

```C#
string input = "Adres e-mail: jan.kowalski@gmail.com";
string pattern = @"\w+@\w+\.\w+";
string output = Regex.Matches(input, pattern)[0].Value;
Console.WriteLine(output); // jan.kowalski@gmail.com
```

## Głębszy zanurzenie:

Pierwsza implementacja wyrażeń regularnych pojawiła się już w latach 50. XX wieku, jednak dopiero w latach 90. zyskała na popularności dzięki wykorzystaniu w języku Perl. Obecnie są one dostępne w wielu językach programowania, a także w edytorach tekstowych czy przeglądarkach internetowych.

Alternatywą dla wyrażeń regularnych są wyrażenia funkcyjne, jednak są one mniej wydajne i trudniejsze w użyciu.

Podczas implementacji wyrażeń regularnych w C# należy pamiętać o kilku ważnych elementach, takich jak specjalne znaki (np. "^" oznacza początek tekstu), kwantyfikatory czy grupy.

## Zobacz także:

- Dokumentacja wyrażeń regularnych w C# na stronie Microsoft: https://docs.microsoft.com/en-us/dotnet/standard/base-types/regular-expression-language-quick-reference
- Przykłady użycia wyrażeń regularnych w celu walidacji danych: https://docs.microsoft.com/en-us/dotnet/standard/base-types/how-to-verify-that-strings-are-in-valid-email-format
- Narzędzie online do testowania wyrażeń regularnych: https://regexr.com/