---
title:                "Wykorzystanie wyrażeń regularnych"
date:                  2024-01-19
simple_title:         "Wykorzystanie wyrażeń regularnych"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Regularne wyrażenia, czyli regexy, to zestaw reguł do wyszukiwania i manipulowania tekstami. Używa się ich, bo pozwalają szybko znajdować wzorce, walidować dane i przeprowadzać złożone operacje na stringach.

## Jak to zrobić:
```C#
using System;
using System.Text.RegularExpressions;

class Program
{
    static void Main()
    {
        string tekst = "Kontakt: jan.kowalski@example.com";
        string wzorzec = @"\b[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Z]{2,6}\b";

        // Sprawdzanie czy tekst pasuje do wzorca
        bool czyPasuje = Regex.IsMatch(tekst, wzorzec);
        Console.WriteLine($"Czy pasuje: {czyPasuje}");  // Wyjście: Czy pasuje: True

        // Wyszukiwanie dopasowania
        Match dopasowanie = Regex.Match(tekst, wzorzec);
        if (dopasowanie.Success)
        {
            Console.WriteLine($"Znaleziony email: {dopasowanie.Value}");  // Wyjście: Znaleziony email: jan.kowalski@example.com
        }

        // Podmiana tekstu
        string zmienionyTekst = Regex.Replace(tekst, wzorzec, "usunięty@kontakt.pl");
        Console.WriteLine(zmienionyTekst);  // Wyjście: Kontakt: usunięty@kontakt.pl
    }
}
```

## Głębiej w temat:
Regularne wyrażenia mają korzenie w teorii automatów i języków formalnych. Powstały w latach 50. XX wieku i są dzisiaj wbudowane w wiele języków programowania i narzędzi. Istnieją alternatywy do regexów, takie jak parsery czy wyrażenia lambda, ale żadna nie oferuje takiej samej elastyczności i wydajności przy pracy z tekstami. Implementacja regexów w .NET używa automatu skończonego, co zapewnia ich wysoką efektywność.

## Zobacz również:
- [Dokumentacja Microsoft dla System.Text.RegularExpressions](https://docs.microsoft.com/pl-pl/dotnet/api/system.text.regularexpressions.regex?view=net-6.0)
- [Tutorial na temat regexów w C#](https://www.dotnetperls.com/regex)
- [Przewodnik po wyrażeniach regularnych](https://www.regular-expressions.info/)
