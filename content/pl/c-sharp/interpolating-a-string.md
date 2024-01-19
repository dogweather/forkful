---
title:                "Interpolacja ciągu znaków"
html_title:           "C++: Interpolacja ciągu znaków"
simple_title:         "Interpolacja ciągu znaków"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Co to jest i dlaczego?

Interpolacja łańcuchów to technika formatowania tekstu w C#, która pozwala na wstawianie zmiennych bezpośrednio w łańcuch. Programiści używają jej, aby zwiększyć czytelność kodu i ułatwić formatowanie.

## Jak to zrobić:

W C#, interpolację łańcuchów wykonuje się za pomocą wyrażeń „$”.

```C#
string imie = "Jan";
string powitanie = $"Witaj, {imie}!";
Console.WriteLine(powitanie); // Wyświetli: Witaj, Jan!
```

Tu, zmienna `imie` jest interpolowana do łańcucha `powitanie`. 

Możemy również interpolować wyrażenia, takie jak:

```C#
int x = 5;
int y = 10;
Console.WriteLine($"Suma {x} i {y} wynosi {x + y}."); // Wyświetli: Suma 5 i 10 wynosi 15.
```

## Deep Dive:

Interpolacja łańcuchów została wprowadzona w C# 6.0 jako ulepszona alternatywa dla funkcji `string.Format`. Interpolacja łańcuchów ma lepszą wydajność od `string.Format` ponieważ stara się minimalizować liczbe alokacji.

Alternatywą dla interpolacji łańcuchów jest używanie konkatenacji łańcuchów, ale jest ona zazwyczaj mniej czytelna i powoduje wiele niepotrzebnych alokacji, które mogą obciążyć system.

Interpolacja działa poprzez przekształcenie łańcucha tekstowego z interpolacją na łańcuch konkatynowany za pomocą operatora `+` albo na `string.Format`, w zależności od użycia.

## Zobacz również:

1. Opracowanie Microsoft na temat interpolacji łańcuchów: 
https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/tokens/interpolated

2. Wykład na YouTube o interpolacji łańcuchów w C#: 
https://www.youtube.com/watch?v=hUzj3FG0IfY

3. Artykuł dotyczący sprawności interpolacji stringów w C#:
https://www.hanselman.com/blog/exploring-c-6s-stringinterpolationperf-improvements