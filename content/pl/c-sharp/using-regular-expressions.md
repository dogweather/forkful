---
title:                "C#: Używanie wyrażeń regularnych"
programming_language: "C#"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/using-regular-expressions.md"
---

{{< edit_this_page >}}

# Dlaczego korzystać z wyrażeń regularnych w C#?

Czy wiesz, że wyrażenia regularne to potężne narzędzie, które może uprościć twój kod i ułatwić ci pracę z tekstem? Dzięki nim możesz szybko i precyzyjnie przetwarzać dane, odwzorowywać wzorce oraz sprawdzać poprawność wprowadzonych danych. W tym artykule dowiesz się, dlaczego warto wykorzystywać wyrażenia regularne w swoim kodzie.

## Jak używać wyrażeń regularnych w C#?

Aby móc korzystać z wyrażeń regularnych w języku C#, musisz najpierw dołączyć przestrzeń nazw `System.Text.RegularExpressions`. Następnie możesz użyć klasy `Regex`, która zawiera wiele przydatnych metod. Przykładowo, jeśli chcesz wykryć wszystkie wystąpienia słowa "programowanie" w tekście, możesz zastosować funkcję `Matches`:

```C#
using System.Text.RegularExpressions;

string tekst = "Uczę się programowania w C#, a ty?";
string wzorzec = "programowanie";

MatchCollection dopasowania = Regex.Matches(tekst, wzorzec);

foreach (Match dopasowanie in dopasowania)
{
    Console.WriteLine("Znaleziono dopasowanie: " + dopasowanie.Value);
}

// Wynik:
// Znaleziono dopasowanie: programowanie
```

W powyższym przykładzie zastosowaliśmy metodę `Matches`, która zwraca kolekcję wszystkich dopasowań wzorca w tekście. Następnie za pomocą pętli `foreach` wypisujemy znalezione dopasowania. Dzięki temu możemy szybko odnaleźć interesujące nas części tekstu.

## Głębszy wgląd w wyrażenia regularne

Wyrażenia regularne w języku C# umożliwiają również wykonywanie bardziej zaawansowanych operacji, takich jak grupowanie i używanie wyrażeń warunkowych. Możesz także wykorzystać wyrażenia regularne wraz z wyrażeniami lambda, co może ułatwić jeszcze bardziej przetwarzanie tekstu.

Ponadto, warto zapoznać się z różnymi składniami i wyrażeniami, które można stosować w wyrażeniach regularnych, aby móc wykorzystać je w różnych przypadkach. Przydatne może być także korzystanie z narzędzi online do sprawdzania poprawności wyrażeń regularnych, takich jak regex101 czy regexr.

# Zobacz także

- [Dokumentacja Microsoft o wyrażeniach regularnych w C#](https://docs.microsoft.com/pl-pl/dotnet/standard/base-types/regular-expression-language-quick-reference)
- [Porównanie wyrażeń regularnych w C# i JavaScript](https://dev.to/bjhaid_93/host-spotlight-net-regex-vs-javascript-regex-49m0)
- [Przykładowe zastosowania wyrażeń regularnych w C#](https://www.tutorialspoint.com/csharp/csharp_regular_expressions.htm)