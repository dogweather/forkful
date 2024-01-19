---
title:                "Konwersja ciągu znaków na małe litery"
html_title:           "Fish Shell: Konwersja ciągu znaków na małe litery"
simple_title:         "Konwersja ciągu znaków na małe litery"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Konwersja łańcucha na małe litery to proces zamiany każdej litery w łańcuchu na jej odpowiednik w małych literach. Programiści robią to, aby ułatwić porównywanie tekstu, ignorując różnicę między dużymi a małymi literami.

## Jak to zrobić:

Podstawowym sposobem konwersji łańcucha na małe litery w C# jest użycie metody `ToLower()`. Poniżej znajduje się przykład:

```C#
string myString = "Hello, World!";
string lowerCaseString = myString.ToLower();

Console.WriteLine(lowerCaseString);
//Wyniki: hello, world!
```

Ta metoda zwraca nowy łańcuch, w którym wszystkie wielkie litery są zastąpione małymi literami.

## Deep Dive

Konwersja tekstu na małe litery jest stosowana od początku czasów informatyki. Niemniej jednak, nowoczesne języki programowania takie jak C# mają wbudowane funkcje, które ułatwiają tę pracę.

Alternatywą dla metody `ToLower()` jest metody `ToLowerInvariant()`. Różnica polega na tym, że `ToLowerInvariant()` jest bardziej odpowiednia dla celów związanych z kulturą niezależną od tekstu, takich jak porównywanie haseł, podczas gdy `ToLower()` jest bardziej odpowiednia dla wyświetlania tekstu.

Warto zauważyć, że metoda `ToLower()` w C# działa na zasadzie tzw. in-place: nie modyfikuje oryginalnego łańcucha, ale zwraca nowy, zmodyfikowany łańcuch. Jest to zgodne z faktem, że stringi w C# są niemutowalne.

## Zobacz również

- MSDN Dokumentacja: [String.ToLower Method](https://docs.microsoft.com/en-us/dotnet/api/system.string.tolower)
- Stack Overflow: [Difference between ToLowerInvariant and ToLower](https://stackoverflow.com/questions/2801508/difference-between-tolowerinvariant-and-tolower) 
- C# Station: [String Manipulation](https://csharp-station.com/Tutorial/CSharp/Lesson08)