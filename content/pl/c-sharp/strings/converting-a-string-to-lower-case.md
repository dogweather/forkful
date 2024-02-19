---
aliases:
- /pl/c-sharp/converting-a-string-to-lower-case/
date: 2024-01-20 17:38:03.379665-07:00
description: "Zmienianie ci\u0105gu znak\xF3w na ma\u0142e litery to proces zamiany\
  \ wszystkich wielkich liter w tek\u015Bcie na ich ma\u0142e odpowiedniki. Programi\u015B\
  ci to robi\u0105, aby\u2026"
lastmod: 2024-02-18 23:08:49.598209
model: gpt-4-1106-preview
summary: "Zmienianie ci\u0105gu znak\xF3w na ma\u0142e litery to proces zamiany wszystkich\
  \ wielkich liter w tek\u015Bcie na ich ma\u0142e odpowiedniki. Programi\u015Bci\
  \ to robi\u0105, aby\u2026"
title: "Konwersja ci\u0105gu znak\xF3w na ma\u0142e litery"
---

{{< edit_this_page >}}

## What & Why? (Co i Dlaczego?)
Zmienianie ciągu znaków na małe litery to proces zamiany wszystkich wielkich liter w tekście na ich małe odpowiedniki. Programiści to robią, aby ujednolicić dane, np. przy porównywaniu ciągów lub przygotowaniu tekstu do wyświetlenia.

## How to: (Jak to zrobić:)
W C# zamiana na małe litery jest prosta dzięki metodzie `ToLower()`. Poniżej przykład użycia:

```C#
using System;

class Program
{
    static void Main()
    {
        string example = "Witaj, Świecie!";
        string lowerCaseExample = example.ToLower();
        
        Console.WriteLine(lowerCaseExample); // wyświetli: "witaj, świecie!"
    }
}
```

## Deep Dive (Dogłębna analiza)
Historia metody `ToLower()` w C# jest powiązana z ewolucją języka i jego funkcji do pracy z tekstami. W przeszłości, alternatywą była ręczna iteracja po znakach i przekształcanie ich przy pomocy mapowania Unicode. 

Alternatywnie, jeśli pracujemy w kontekście międzynarodowym, lepiej użyć `ToLowerInvariant()`, które ignoruje ustawienia regionalne i zapewnia spójność wyników.

Implementacja `ToLower()` w .NET używa informacji o lokalizacji (CultureInfo) aby określić, jak przekształcić każdy znak - to ma znaczenie dla alfabetów innych niż łaciński.

## See Also (Zobacz również)
- Dokumentacja Microsoft na temat metody `ToLower()`: [https://docs.microsoft.com/dotnet/api/system.string.tolower](https://docs.microsoft.com/dotnet/api/system.string.tolower)
- Dokumentacja Microsoft na temat kulturowych informacji (`CultureInfo`): [https://docs.microsoft.com/en-us/dotnet/api/system.globalization.cultureinfo](https://docs.microsoft.com/en-us/dotnet/api/system.globalization.cultureinfo)
- Stack Overflow - dyskusje i problemy dotyczące przekształcania ciągów znaków: [https://stackoverflow.com/questions/tagged/c%23+lowercase](https://stackoverflow.com/questions/tagged/c%23+lowercase)
