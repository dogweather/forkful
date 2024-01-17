---
title:                "Interpolowanie ciągu znaków"
html_title:           "C#: Interpolowanie ciągu znaków"
simple_title:         "Interpolowanie ciągu znaków"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?
Interpolowanie łańcuchów jest ważnym elementem w języku C#, umożliwiającym łączenie łańcuchów tekstowych z innymi typami danych. Programiści często używają tej funkcji, aby wyświetlać zmienną lub wyrażenie wewnątrz tekstu.

## Jak to zrobić?
Możesz użyć znaku dolara ($) przed otwarciem nawiasu klamrowego, aby określić, że chcesz użyć interpolowanego łańcucha. Na przykład:

```C#
string name = "Jan";
Console.WriteLine($"Cześć, nazywam się {name}."); // Output: Cześć, nazywam się Jan.
```

Możesz również użyć interpolacji łańcucha w wyrażeniu warunkowym. Na przykład:

```C#
int age = 25;
Console.WriteLine($"Jesteś { (age >= 18 ? "pełnoletni" : "niepełnoletni") }."); // Output: Jesteś pełnoletni.
```

## Głębsze zagadnienia
Interpolacja łańcuchów została wprowadzona w C# 6.0 jako alternatywa dla metody `String.Format()`. Jest bardziej czytelna i wygodniejsza w użyciu. Istnieją również biblioteki strony trzeciej, takie jak `StringInterpolationHelpers`, które dodają dodatkowe funkcje, takie jak formatowanie i tłumaczenie interpolowanych łańcuchów.

## Zobacz także
- [Dokumentacja Microsoft na temat interpolacji łańcuchów w C#](https://docs.microsoft.com/pl-pl/dotnet/csharp/language-reference/tokens/interpolated)
- [Przykładowe zastosowania interpolacji łańcuchów](https://www.c-sharpcorner.com/UploadFile/8911c4/interpolated-strings-in-c-sharp/)
- [Biblioteka StringInterpolationHelpers](https://github.com/haacked/StringInterpolationHelpers)