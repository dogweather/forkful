---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:10:08.647715-07:00
description: "Tablice asocjacyjne, czyli s\u0142owniki w C#, pozwalaj\u0105 przechowywa\u0107\
  \ i zarz\u0105dza\u0107 parami kluczy i warto\u015Bci. S\u0105 Twoim pierwszym wyborem,\
  \ gdy potrzebujesz szybko\u2026"
lastmod: '2024-03-11T00:14:08.583791-06:00'
model: gpt-4-0125-preview
summary: "Tablice asocjacyjne, czyli s\u0142owniki w C#, pozwalaj\u0105 przechowywa\u0107\
  \ i zarz\u0105dza\u0107 parami kluczy i warto\u015Bci. S\u0105 Twoim pierwszym wyborem,\
  \ gdy potrzebujesz szybko\u2026"
title: Korzystanie z tablic asocjacyjnych
---

{{< edit_this_page >}}

## Co i dlaczego?

Tablice asocjacyjne, czyli słowniki w C#, pozwalają przechowywać i zarządzać parami kluczy i wartości. Są Twoim pierwszym wyborem, gdy potrzebujesz szybko pobierać wartości na podstawie unikatowego identyfikatora, co ułatwia zarządzanie danymi w skomplikowanych aplikacjach.

## Jak używać:

W C# pracujesz z tablicami asocjacyjnymi za pomocą klasy `Dictionary<TKey, TValue>`. Oto krótki przykład, aby zacząć:

```C#
using System;
using System.Collections.Generic;

class Program
{
    static void Main()
    {
        // Tworzenie słownika
        Dictionary<string, int> koszykZowocami = new Dictionary<string, int>();

        // Dodawanie par klucz-wartość
        koszykZowocami.Add("Jabłka", 5);
        koszykZowocami.Add("Pomarańcze", 10);

        // Dostęp do wartości za pomocą klucza
        Console.WriteLine("Jabłka: " + koszykZowocami["Jabłka"]);
        
        // Aktualizacja wartości
        koszykZowocami["Jabłka"] = 7;
        Console.WriteLine("Zaktualizowane Jabłka: " + koszykZowocami["Jabłka"]);
        
        // Usuwanie pary klucz-wartość
        koszykZowocami.Remove("Pomarańcze");

        // Iteracja przez słownik
        foreach (var para in koszykZowocami)
        {
            Console.WriteLine(para.Key + ": " + para.Value);
        }
    }
}
```
Przykładowe wyjście:
```
Jabłka: 5
Zaktualizowane Jabłka: 7
Jabłka: 7
```

Przykład pokazuje tworzenie słownika, dodawanie, dostęp, aktualizację i usuwanie elementów, oraz iterację przez niego.

## Szczegółowe omówienie

Koncepcja tablic asocjacyjnych sięga ich użycia w językach skryptowych takich jak Perl i PHP, gdzie oferują one elastyczność w zarządzaniu kolekcjami danych. W C#, `Dictionary<TKey, TValue>` jest faktyczną implementacją, wprowadzoną w .NET Framework 2.0. Przechowuje dane w tablicy haszującej, zapewniając efektywne wyszukiwanie, dodawanie i usuwanie.

Jednakże warto zauważyć, że mimo iż słowniki są niezwykle wszechstronne, nie zawsze mogą być najlepszym rozwiązaniem. Dla utrzymania uporządkowanych kolekcji, możesz rozważyć `SortedDictionary<TKey, TValue>` lub `SortedList<TKey, TValue>`, które oferują uporządkowanie kosztem wolniejszych operacji wstawiania i usuwania. W scenariuszach wymagających bezpieczeństwa wątków, `ConcurrentDictionary<TKey, TValue>` dodaje obciążenie, ale zapewnia bezpieczny dostęp z wielu wątków bez ręcznego blokowania.

Ostatecznie wybór implementacji tablicy asocjacyjnej w C# zależy od Twoich konkretnych potrzeb dotyczących kolejności, wydajności i bezpieczeństwa wątkowego.
