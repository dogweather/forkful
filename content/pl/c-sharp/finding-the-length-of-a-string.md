---
title:                "C#: Znalezienie długości ciągu znaków"
simple_title:         "Znalezienie długości ciągu znaków"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Często zdarza się, że w trakcie programowania musimy operować na różnych napisach. Przydaje się wtedy znajomość długości tych napisów, aby móc efektywnie manipulować nimi w kodzie. W tym artykule dowiesz się jak w prosty sposób obliczać długość stringów w języku C#.

## Jak To Zrobić

Aby obliczyć długość stringa w C#, możemy skorzystać z metody `Length`. Przykładowo, jeśli chcemy wyświetlić długość napisu "Hello World", możemy to zrobić za pomocą poniższego kodu:

```C# 
string text = "Hello World";
Console.WriteLine("Długość napisu to: " + text.Length);
```

W wyniku otrzymamy wartość 11, ponieważ string składa się z 11 znaków (w tym spacja). Metoda `Length` zwraca wartość całkowitą, więc możemy wykorzystać ją również do warunków logicznych:

```C#
string name = "Anna";
if (name.Length > 5)
{
    Console.WriteLine("Twoje imię jest dość długie!");
}
```

W tym przykładzie, jeśli imię będzie dłuższe niż 5 znaków, zostanie wyświetlona odpowiednia wiadomość.

## Deep Dive

Warto zauważyć, że metoda `Length` liczy również znaki specjalne, takie jak spacje, przecinki czy kropki. W przypadku, gdy chcemy pominąć te znaki, możemy skorzystać z metody `Trim` lub zignorować je w pętli za pomocą warunku `if`.

## Zobacz również

- Dokumentacja Microsoft o metodzie `Length`: https://docs.microsoft.com/en-us/dotnet/api/system.string.length?view=netcore-3.1
- Przykłady użycia metody `Length`: https://www.c-sharpcorner.com/blogs/how-to-find-length-of-the-string-in-c-sharp-programming1