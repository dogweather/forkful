---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:10:08.647715-07:00
description: "Jak u\u017Cywa\u0107: W C# pracujesz z tablicami asocjacyjnymi za pomoc\u0105\
  \ klasy `Dictionary<TKey, TValue>`. Oto kr\xF3tki przyk\u0142ad, aby zacz\u0105\u0107\
  ."
lastmod: '2024-03-13T22:44:35.401377-06:00'
model: gpt-4-0125-preview
summary: "W C# pracujesz z tablicami asocjacyjnymi za pomoc\u0105 klasy `Dictionary<TKey,\
  \ TValue>`."
title: Korzystanie z tablic asocjacyjnych
weight: 15
---

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
