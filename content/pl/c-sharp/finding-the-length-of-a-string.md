---
title:                "Znajdowanie długości ciągu znaków"
html_title:           "Arduino: Znajdowanie długości ciągu znaków"
simple_title:         "Znajdowanie długości ciągu znaków"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Co i dlaczego? 
Znalezienie długości ciągu służy do oznaczenia ilości znaków w danym ciągu. Programiści to robią, aby kontrolować rozmiar danych wejściowych i uniknąć błędów podczas manipulacji ciągami.

## Jak to zrobić: 
Oto prosty przykład kodu C#, który pokazuje, jak znaleźć długość ciągu:

```C#
string mojCiąg = "Dzień dobry, Świecie!";
int długośćCiągu = mojCiąg.Length;
Console.WriteLine(długośćCiągu);
```

Wynik:

```text
21
```

## Głębsze spojrzenie: 

(1) Historyczny kontekst: Wcześniejsze wersje C# nie miały wbudowanego operatora `.Length` do znalezienia długości ciągu. Zamiast tego, programiści musieli napisać metody iterując po ciągu znak po znaku.

(2) Alternatywy: Można również używać metody `.Count()` z przestrzeni nazw System.Linq. Jednak jest ona wolniejsza, ponieważ iteruje przez ciąg, zamiast odczytać wartość z właściwości.

```C#
using System.Linq;   
...
string mojCiąg = "Dzień dobry, Świecie!";
int długośćCiągu = mojCiąg.Count();
Console.WriteLine(długośćCiągu);
```

(3) Szczegóły implementacji: W C# długość ciągu jest przechowywana jako wartość 32-bitowa, więc maksymalna długość ciągu której może przechować wynosi `int.MaxValue`, czyli dokładnie 2,147,483,647.

## Zobacz także: 

1. [C# String Length Property](https://docs.microsoft.com/pl-pl/dotnet/api/system.string.length) 
2. [Stack Overflow Discussion on String Length](https://stackoverflow.com/questions/1483919/what-is-string-length-in-c-sharp) 
3. [C# String Count Method](https://docs.microsoft.com/pl-pl/dotnet/api/system.linq.enumerable.count)