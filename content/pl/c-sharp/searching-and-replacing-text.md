---
title:                "Wyszukiwanie i zastępowanie tekstu"
html_title:           "Javascript: Wyszukiwanie i zastępowanie tekstu"
simple_title:         "Wyszukiwanie i zastępowanie tekstu"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# Szukanie i zamiana tekstu w C#: proste i szybko

## Co i dlaczego?
Szukanie i zamiana tekstu to podstawowe operacje, które pozwalają na manipulowanie danymi tekstowymi. Programiści korzystają z nich codziennie, aby poprawić efektywność i zautomatyzować monotonne zadania.

## Jak to zrobić:
W C# możemy użyć metody `Replace` do wymiany ciągów tekstowych. Poniżej znajduje się przykład kodu.

```C#
using System;

class Program
{
    static void Main()
    {
        const string tekst = "Cześć, jak się masz?";
        const string szukać = "masz";
        const string zamienić = "czujesz";

        string wynik = tekst.Replace(szukać, zamienić);

        Console.WriteLine(wynik);  // Wypisuje: "Cześć, jak się czujesz?"
    }
}
```

## Wgłębne informacje

1. **Kontekst historyczny:** Szukanie i zamiana tekstu to jedne z najstarszych funkcji w programowaniu, istnieją od czasów języka Assembly. Chociaż techniki i metody uległy zmianie, podstawowy koncept pozostaje taki sam.
2. **Alternatywy:** Oprócz metody `Replace`, programiści mogą korzystać z wyrażeń regularnych (`Regex`) do bardziej skomplikowanych zastąpień. Wyrażenia regularne oferują większą elastyczność, ale są również trudniejsze do opanowania.
3. **Szczegóły implementacji:** Metoda `Replace` działa poprzez iterowanie przez ciąg, porównywanie podciągów i zastępowanie dopasowań. To proste, ale efektywne podejście, które dobrze działa dla większości zastosowań.

## Zobacz także

Jeśli chcesz dowiedzieć się więcej o szukaniu i zamianie tekstu w C#, sprawdź poniższe źródła:

1. ["String.Replace Method" (Microsoft Docs)](https://docs.microsoft.com/pl-pl/dotnet/api/system.string.replace?view=net-5.0)
3. ["Manipulacja ciągami w C#" (tutorialsteacher.com)](https://www.tutorialsteacher.com/csharp/csharp-string)

+-