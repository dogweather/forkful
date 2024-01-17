---
title:                "Zmiana wielkości liter w ciągu znaków"
html_title:           "C#: Zmiana wielkości liter w ciągu znaków"
simple_title:         "Zmiana wielkości liter w ciągu znaków"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Kapitalizacja ciągu znakowego to zmiana pierwszej litery każdego słowa na dużą. Programiści często wykorzystują to w swoim kodzie, aby poprawić czytelność i estetykę wyświetlanych wiadomości lub etykiet.

## Jak to zrobić:
Aby kapitalizować ciąg znakowy w C#, możesz użyć metody `ToUpper` lub `ToTitleCase`. Przykład:

```C#
string message = "witaj, programisto!";
Console.WriteLine(message.ToUpper());
// Output: WITAJ, PROGRAMISTO!
```

```C#
string label = "opcje użytkownika";
Console.WriteLine(CultureInfo.CurrentCulture.TextInfo.ToTitleCase(label));
// Output: Opcje Użytkownika
```

## Głębsze zanurzenie:
Metoda `ToUpper` jest dostępna w C# już od wersji 1.0, natomiast `ToTitleCase` pojawiła się dopiero w wersji 4.0. Alternatywą dla tych metod jest użycie pętli i ręcznego zmieniania pierwszych liter w ciągu znaków. Jest to jednak mniej wydajne i bardziej podatne na błędy.

Jeśli chcesz kapitalizować tylko pierwszą literę całego ciągu, możesz użyć metody `ToTitleCase` w połączeniu z `Substring` i `ToLower`. Aby dodatkowo uniknąć błędów związanych z wielokulturowością, zaleca się użycie klasy `TextInfo` zdefiniowanej w klasie `CultureInfo`.

## Zobacz także:
- [Dokumentacja C# dla metody ToUpper](https://docs.microsoft.com/pl-pl/dotnet/api/system.string.toupper?view=net-5.0)
- [Dokumentacja C# dla metody ToTitleCase](https://docs.microsoft.com/pl-pl/dotnet/api/system.globalization.textinfo.totitlecase?view=net-5.0)
- [Pomoc techniczna Microsoft dla klasy TextInfo](https://docs.microsoft.com/pl-pl/dotnet/api/system.globalization.textinfo?view=net-5.0)