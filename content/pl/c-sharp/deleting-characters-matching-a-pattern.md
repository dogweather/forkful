---
title:                "Usuwanie znaków pasujących do wzorca"
html_title:           "C#: Usuwanie znaków pasujących do wzorca"
simple_title:         "Usuwanie znaków pasujących do wzorca"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?

Usuwanie znaków pasujących do wzorca jest częstym zadaniem podczas pisania kodu w C#. Polega ono na wyeliminowaniu określonych znaków z łańcucha znaków, aby dostosować go do określonych wymagań. Programiści często wykonują tę operację, ponieważ pomaga im to w czyszczeniu i formatowaniu danych wejściowych.

## Jak to zrobić:

#### Przykład 1:
```C#
string input = "123-456-789";
string output = Regex.Replace(input, "-", "");

Console.WriteLine(output);
```
#### Wyjście:
123456789

#### Przykład 2:
```C#
string input = "1A2B3C4D5E";
string output = Regex.Replace(input, @"\D", "");

Console.WriteLine(output);
```
#### Wyjście:
12345

## Głębsza Analiza

Usuwanie znaków pasujących do wzorca nie jest nowym konceptem. W rzeczywistości, podczas programowania często używamy tego w różnych formach. Jedną z popularnych metod w C# jest użycie metody Regex.Replace(), która pozwala na znalezienie oraz zastąpienie określonych znaków za pomocą wybranego wzorca.

Alternatywnym sposobem na usuwanie znaków pasujących do wzorca jest użycie pętli oraz warunków, aby przeprowadzić iterację po każdym znaku i wykluczyć te, które nie pasują do wybranego wzorca. Ta metoda jest bardziej czasochłonna, ale daje większą kontrolę nad usuwaniem znaków.

Implementacja usuwania znaków pasujących do wzorca może się różnić w zależności od języka programowania. Jednak w C# zawsze możemy polegać na wygodnej i efektywnej bibliotece Regex, która oferuje wiele metod do manipulacji łańcuchami znaków.

## Zobacz również:

- [Dokumentacja Microsoft do metody Regex.Replace()](https://docs.microsoft.com/en-us/dotnet/api/system.text.regularexpressions.regex.replace?view=netcore-3.1)
- [Poradnik do pracy z wyrażeniami regularnymi w C#](https://www.c-sharpcorner.com/blogs/basics-of-regular-expression-regularexpression-in-c-sharp1)