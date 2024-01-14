---
title:                "C#: Łączenie ciągów znaków"
programming_language: "C#"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/concatenating-strings.md"
---

{{< edit_this_page >}}

# Dlaczego warto używać łączenia łańcuchów w języku C#?

Język C# oferuje wiele różnych funkcji, które ułatwiają pracę z łańcuchami znaków. Jedną z nich jest łączenie, czyli łączenie dwóch lub więcej łańcuchów w jeden. Jest to niezwykle przydatna funkcja w różnych rodzajach programów, od prostych aplikacji konsolowych po skomplikowane aplikacje webowe.

## Jak używać łączenia łańcuchów w języku C#?

Aby skorzystać z funkcji łączenia w języku C#, należy użyć operatora "+" i podać dwa lub więcej łańcuchów wewnątrz nawiasów. Na przykład:

```C#
string firstName = "Jan";
string lastName = "Kowalski";
string fullName = firstName + " " + lastName;
Console.WriteLine(fullName);
```

W powyższym przykładzie połączyliśmy zmienne "firstName" i "lastName" za pomocą operatora "+". Następnie, używając funkcji "Console.WriteLine", wyświetliliśmy ich połączenie, czyli pełne imię i nazwisko. Konsola wyświetli "Jan Kowalski".

Możemy również łączyć więcej niż dwa łańcuchy za pomocą operatora "+". Na przykład:

```C#
string sentence = "Witaj ";
string firstName = "Jan";
string greeting = sentence + firstName + "! Jak się masz?";
Console.WriteLine(greeting);
```

Takie wykorzystanie łączenia może być bardzo przydatne przy tworzeniu dynamicznych wiadomości lub komunikatów dla użytkowników naszej aplikacji.

## Podstawowa zasada działania łączenia łańcuchów w języku C#

Podczas łączenia łańcuchów w języku C#, wszystkie zmienne zawierające łańcuchy znaków są konkatenowane w jedną zmienną. Oznacza to, że zmienne te nie są zmieniane, a jedynie tworzony jest nowy łańcuch z połączonymi wartościami. Jest to istotne, gdyż zmienne zawierające łańcuchy znaków są niezmiennicze (immutable), co oznacza, że nie można bezpośrednio zmieniać ich wartości, tylko tworzyć nowe zmienne zawierające zmienione lub połączone łańcuchy.

## Zobacz także

- [Dokumentacja Microsoft na temat łączenia łańcuchów w języku C#](https://docs.microsoft.com/pl-pl/dotnet/csharp/programming-guide/strings/#concatenating)
- [Przykłady i wyjaśnienia operatora "+" w języku C#](https://www.pluralsight.com/guides/working-with-strings-csharp)
- [Poradnik na temat wielowątkowego łączenia łańcuchów w języku C#](https://stackify.com/formatting-strings-in-csharp/)