---
date: 2024-01-20 17:57:48.915717-07:00
description: "Szukanie i zamiana tekstu to codzienny chleb programisty. Robimy to,\
  \ aby modyfikowa\u0107 dane, poprawia\u0107 b\u0142\u0119dy, czy te\u017C dostosowywa\u0107\
  \ ci\u0105gi znak\xF3w pod\u2026"
lastmod: '2024-03-13T22:44:35.392561-06:00'
model: gpt-4-1106-preview
summary: "Szukanie i zamiana tekstu to codzienny chleb programisty. Robimy to, aby\
  \ modyfikowa\u0107 dane, poprawia\u0107 b\u0142\u0119dy, czy te\u017C dostosowywa\u0107\
  \ ci\u0105gi znak\xF3w pod\u2026"
title: Wyszukiwanie i zamiana tekstu
---

{{< edit_this_page >}}

## Co i dlaczego?
Szukanie i zamiana tekstu to codzienny chleb programisty. Robimy to, aby modyfikować dane, poprawiać błędy, czy też dostosowywać ciągi znaków pod konkretne potrzeby. Proste, ale niezwykle istotne narzędzie w naszej kuchni kodu.

## Jak to zrobić:
W C# używamy głównie klasy `String`. Oto jak szukać i zamieniać tekst:

```C#
string source = "Witaj świecie! Hello World!";
string toFind = "świecie";
string replaceWith = "programisto";

// Zamiana tekstu
string replaced = source.Replace(toFind, replaceWith);
Console.WriteLine(replaced);  // Wynik: "Witaj programisto! Hello World!"
```

A co jeśli chcesz coś bardziej zaawansowanego? Na przykład, zamienić wyłącznie pierwsze wystąpienie frazy. Regułki (regex) wchodzą do gry:

```C#
using System.Text.RegularExpressions;

string text = "Kot, kot, coś tam kot.";
string pattern = "kot";
string replacement = "pies";
string result = Regex.Replace(text, pattern, replacement, RegexOptions.IgnoreCase, TimeSpan.FromMilliseconds(500));

Console.WriteLine(result);  // Wynik: "Pies, kot, coś tam kot."
```

To tylko ignoryje wielkość liter i zamienia pierwsze wystąpienie, dzięki parametrowi `RegexOptions.IgnoreCase`.

## Deep Dive
Szukanie i zamiana tekstu to funkcjonalności które miały swoje początki w edytorach tekstu lat 60-tych. Obecne implementacje, jak regex, pochodzą z prac Stephena Cole'a Kleene'a.

Alternatywy? Jak najbardziej. Pluginy do IDE, narzędzia do obsługi tekstów jak sed, awk w Unix lub PowerShell w Windows. Składnia różni się, ale idea ta sama.

Szukanie i zamiana w C# pod spodem działają na tablicach znaków. Ma to wpływ na wydajność – szczególnie przy dużych ciągach tekstowych.

## See Also
- [Microsoft's official String documentation](https://docs.microsoft.com/en-us/dotnet/api/system.string?view=netcore-3.1)
- [Regex class documentation](https://docs.microsoft.com/en-us/dotnet/api/system.text.regularexpressions.regex?view=netcore-3.1)
- [Informacje o wyrażeniach regularnych](https://docs.microsoft.com/pl-pl/dotnet/standard/base-types/regular-expressions) 

Co dalej? Eksperymentuj. Może dodaj odrobinę LINQ do przeszukiwania kolekcji tekstu. Przede wszystkim jednak, baw się swoim kodem. To najlepsza metoda nauki.
