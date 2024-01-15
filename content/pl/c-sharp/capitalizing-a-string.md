---
title:                "Zamiana pierwszej litery na wielką w ciągu znaków."
html_title:           "C#: Zamiana pierwszej litery na wielką w ciągu znaków."
simple_title:         "Zamiana pierwszej litery na wielką w ciągu znaków."
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Zamiana liter na wielkie to często wykonywana operacja w programowaniu, która pozwala na jednolite wyglądanie tekstu. Jest szczególnie przydatna przy pracy z tekstem wprowadzonym przez użytkownika, gdzie nie zawsze poprawnie uformowane dane mogą wpłynąć na poprawne działanie aplikacji. Dzięki tej prostej funkcji możemy również dodać do tekstu niezbędne akcenty.

## Jak to zrobić

W języku C# możemy skorzystać z metody `ToUpper` aby zmienić wszystkie litery w tekście na jego wersję z wielkimi literami. Przyjrzyjmy się poniższemu przykładowi:

```C#
string tekst = "witaj świecie";
string tekstWielkimi = tekst.ToUpper();

Console.WriteLine(tekstWielkimi);

// Output: WITAJ ŚWIECIE
```

W powyższym przykładzie najpierw definiujemy zmienną `tekst` przechowującą nasz tekst, następnie wykorzystujemy metodę `ToUpper` aby utworzyć zmienną `tekstWielkimi` z naszym tekstem w wersji z dużymi literami. Na koniec wyświetlamy zmieniony tekst w konsoli.

Możemy również wykorzystać indeksowanie na stringach, aby zmienić konkretną literę na wielką. Przykład poniżej:

```C#
string tekst = "witaj świecie";

var pierwszaLitera = tekst[0].ToString().ToUpper();
var calyTekstWielkimi = pierwszaLitera + tekst.Substring(1);

Console.WriteLine(calyTekstWielkimi);

// Output: Witaj świecie
```

W pierwszym wierszu definiujemy zmienną `pierwszaLitera` jako pierwszą literę naszego tekstu, która jest zamieniana na wersję z wielkiej litery. Następnie konkatenujemy ją z pozostałym tekstem, który jest pobierany za pomocą metody `Substring`. W ten sposób wyświetlamy tekst z poprawnie uformowanym akcentem.

## Głęboki zanurzenie

Zamiana liter na wielkie jest również możliwa do wykonania dla każdego języka, nie tylko polskiego. Możemy również skorzystać z innych metod w języku C# takich jak `ToTitleCase` aby zmienić pierwszą literę każdego wyrazu na wielką. Warto również pamiętać, że zmiana wielkości liter może również wpłynąć na wydajność aplikacji, dlatego zaleca się dokładną analizę potrzebne dla danej sytuacji.

## Zobacz też

- [Dokumentacja metody ToUpper w języku C#](https://docs.microsoft.com/en-us/dotnet/api/system.string.toupper?view=net-5.0)
- [Dokumentacja metody ToTitleCase w języku C#](https://docs.microsoft.com/en-us/dotnet/api/system.stringtotitlecase?view=net-5.0)