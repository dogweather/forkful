---
date: 2024-01-20 17:38:03.379665-07:00
description: "How to: (Jak to zrobi\u0107:) W C# zamiana na ma\u0142e litery jest\
  \ prosta dzi\u0119ki metodzie `ToLower()`. Poni\u017Cej przyk\u0142ad u\u017Cycia."
lastmod: '2024-04-05T21:53:36.831370-06:00'
model: gpt-4-1106-preview
summary: "(Jak to zrobi\u0107:) W C# zamiana na ma\u0142e litery jest prosta dzi\u0119\
  ki metodzie `ToLower()`."
title: "Konwersja ci\u0105gu znak\xF3w na ma\u0142e litery"
weight: 4
---

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
