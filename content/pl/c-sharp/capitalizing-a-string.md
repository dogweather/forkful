---
title:                "Zamiana liter na wielkie w ciągu znaków"
html_title:           "C#: Zamiana liter na wielkie w ciągu znaków"
simple_title:         "Zamiana liter na wielkie w ciągu znaków"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# Poradnik do Kapitalizacji Tekstu w języku C# 

## Co i Dlaczego?
Kapitalizacja tekstu oznacza konwersję pierwszych liter każdego słowa do wielkich liter. Programiści robią to z wielu powodów, takich jak poprawa czytelności i zgodność z konwencjami dotyczącymi tytułów.

## Jak to zrobić:
Metodą kapitalizacji w C# jest `TextInfo.ToTitleCase`. Poniżej znajduje się przykładowy kod:
```C#
using System;
using System.Globalization;

class MainClass {
  public static void Main (string[] args) {
    TextInfo myTI = new CultureInfo("en-US",false).TextInfo;
    string myString = "witaj świecie";
    string myCapitalizedString = myTI.ToTitleCase(myString);
    Console.WriteLine (myCapitalizedString);
  }
}

```
Output:
```
Witaj Świecie
```
## Dogłębne zrozumienie
C# nie zawiera wbudowanej metody do kapitalizacji, ale `ToTitleCase` w klasie `TextInfo` jest najbliższym odpowiednikiem. 

Projektanci języka C# mogli zdecydować, że nie ma potrzeby włączenia takiej funkcji z naruszeniem zasady jednego odpowiedzialnego za najmniejszą możliwą funkcję.

Alternatywą dla użycia `ToTitleCase` jest napisanie własnej funkcji lub korzystanie z bibliotek zewnętrznych, które oferują tę funkcję.

Ważnym elementem projektu `ToTitleCase` jest to, że nie zmienia wielkości liter w środku słów. Dlatego "iPhone" zostanie zachowane, zamiast stawać się "IPhone".

## Zobacz także
- Microsoft Documentation on TextInfo.ToTitleCase: https://docs.microsoft.com/en-us/dotnet/api/system.globalization.textinfo.totitlecase
- StackOverflow Discussion on Capitalizing in C#: https://stackoverflow.com/questions/4483886/how-can-i-convert-a-string-to-title-case