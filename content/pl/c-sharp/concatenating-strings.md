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

## Co i dlaczego?
Konkatenacja ciągów znaków w programowaniu oznacza łączenie wielu ciągów znaków w jeden dłuższy ciąg. Programiści często wykorzystują to narzędzie w celu tworzenia czytelniejszych i bardziej złożonych napisów, np. w formularzach czy komunikatach dla użytkownika.

## Jak to zrobić?
W języku C# konkatenacja ciągów znaków jest bardzo prosta dzięki operatorowi „+”. Wystarczy umieścić pomiędzy dwoma ciągami znaków ten operator, a program połączy je w jeden ciąg. Przykładowe użycie wygląda tak:
```C#
string imie = "Anna";
string nazwisko = "Kowalska";
string pelneNazwisko = imie + " " + nazwisko;
```
W efekcie zmienna "pelneNazwisko" będzie przechowywała wartość "Anna Kowalska". 

## Głębszy zanurzanie się
Konkatenacja ciągów znaków jest wykorzystywana już od czasów pierwszych języków programowania. W niektórych językach, np. C, wymagała ona użycia specjalnych funkcji lub bibliotek. Jednak w języku C# jest to znacznie ułatwione dzięki wykorzystaniu operatora „+”. Alternatywnym sposobem łączenia ciągów znaków jest wykorzystanie klasy StringBuilder, która jest bardziej efektywna w przypadku łączenia większej liczby ciągów. 

## Zobacz także
- Dokumentacja dotycząca konkatenacji ciągów znaków w języku C#: https://docs.microsoft.com/pl-pl/dotnet/csharp/how-to/concatenate-multiple-strings
- Porównanie wydajności konkatenacji ciągów znaków z użyciem operatora „+” i klasy StringBuilder: https://stackoverflow.com/questions/24718392/performance-difference-between-stringbuilder-and-string-concatenation
- Alternatywne sposoby łączenia ciągów znaków w języku C#: https://www.dotnetperls.com/concat