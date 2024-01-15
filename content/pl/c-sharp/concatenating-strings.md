---
title:                "Łączenie ciągów znaków"
html_title:           "C#: Łączenie ciągów znaków"
simple_title:         "Łączenie ciągów znaków"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/concatenating-strings.md"
---

{{< edit_this_page >}}

## Dlaczego

Często zdarza się, że w programowaniu musimy łączyć różne ciągi znaków w jedną całość. Na przykład, gdy chcemy wyświetlić imię i nazwisko użytkownika, musimy połączyć dwa osobne ciągi znaków w jeden. W tym artykule dowiesz się, jak w prosty sposób połączyć ze sobą różne ciągi znaków w języku C#.

## Jak to zrobić

Aby połączyć ze sobą dwa ciągi znaków w języku C#, musimy skorzystać z metody `Concat()` dostępnej w klasie `String`. Przykładowy kod wyglądałby następująco:

```C#
string firstName = "Jan";
string lastName = "Kowalski";
string fullName = String.Concat(firstName, " ", lastName);
Console.WriteLine(fullName);
```
Output:
```
Jan Kowalski
```

## Dogłębne zagłębienie

Metoda `Concat()` w języku C# jest jednym z wielu sposobów na łączenie ciągów znaków. Istnieje również wiele innych metod, takich jak `String.Join()` czy `StringBuilder`, które pozwolą nam na bardziej efektywne i elastyczne łączenie ciągów. Ważne jest jednak, aby pamiętać o tym, że częste używanie tych metod może wpłynąć na wydajność naszego programu.

## Zobacz również

- Dokumentacja metody `Concat()` w języku C#: https://docs.microsoft.com/pl-pl/dotnet/api/system.string.concat?view=netcore-3.1
- Polecenie `String.Join()` w języku C#: https://docs.microsoft.com/pl-pl/dotnet/api/system.string.join?view=netcore-3.1
- Klasy `StringBuilder` w języku C#: https://docs.microsoft.com/pl-pl/dotnet/api/system.text.stringbuilder?view=netcore-3.1