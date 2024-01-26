---
title:                "Generowanie liczb losowych"
date:                  2024-01-20T17:49:12.612766-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generowanie liczb losowych"
programming_language: "C#"
category:             "C#"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why? (Co i Dlaczego?)
Tworzenie liczb losowych to serce wielu aplikacji - od gier po bezpieczeństwo. Daje nieprzewidywalność potrzebną do symulacji, testów, czy kryptografii.

## How to: (Jak to zrobić:)
```C#
using System;

class Program
{
    static void Main()
    {
        Random random = new Random();
        int randomInt = random.Next(1, 100); // Losowa liczba od 1 do 99
        Console.WriteLine(randomInt); // Przykład wyjścia: 42

        double randomDouble = random.NextDouble(); // Losowa liczba double od 0.0 do 1.0
        Console.WriteLine(randomDouble); // Przykład wyjścia: 0.843103
    }
}
```

## Deep Dive (Głębsze zanurzenie)
Generowanie liczb losowych sięga dalej niż C#. W starożytnej historii kostki do gry były używane do przepowiadania losu. W informatyce, to algorytmy losujące robią robotę. C# używa generatora Mersenne Twistera za kulisami, ale możesz też użyć `System.Security.Cryptography` dla większego bezpieczeństwa.

Alternatywy? `GUID` może być losowy, ale nie nadaje się do celów matematycznych. Jest też `System.Random`, który jest wystarczający dla nie-krytycznych zastosowań.

Szczegóły implementacji? `Random` korzysta z ziarna (ang. seed), które determinuje serię. Bez wskazania ziarna, używany jest czas systemowy. Ale uwaga: tworzenie nowych instancji `Random` w krótkich odstępach czasu może powodować powtarzające się wzorce.

## See Also (Zobacz też)
- Microsoft Documentation: [System.Random](https://docs.microsoft.com/en-us/dotnet/api/system.random?view=net-6.0)
- Stack Overflow: Wątek o [generowaniu bezpiecznych liczb losowych w C#](https://stackoverflow.com/questions/4242648/secure-random-number-generation-in-c-sharp)
- Blog: [Zrozumienie Random w C#](https://devblogs.microsoft.com/pfxteam/getting-random-numbers-in-a-thread-safe-way/)
