---
aliases:
- /pl/c-sharp/generating-random-numbers/
date: 2024-01-27 20:33:06.460666-07:00
description: "Generowanie losowych liczb w C# polega na tworzeniu nieprzewidywalnych\
  \ warto\u015Bci numerycznych w okre\u015Blonym zakresie. Programi\u015Bci u\u017C\
  ywaj\u0105 tych metod do\u2026"
lastmod: 2024-02-18 23:08:49.606567
model: gpt-4-0125-preview
summary: "Generowanie losowych liczb w C# polega na tworzeniu nieprzewidywalnych warto\u015B\
  ci numerycznych w okre\u015Blonym zakresie. Programi\u015Bci u\u017Cywaj\u0105 tych\
  \ metod do\u2026"
title: Generowanie liczb losowych
---

{{< edit_this_page >}}

## Co i Dlaczego?

Generowanie losowych liczb w C# polega na tworzeniu nieprzewidywalnych wartości numerycznych w określonym zakresie. Programiści używają tych metod do implementacji funkcji takich jak kryptografia, symulacje i gry, gdzie wymagana jest nieprzewidywalność lub symulacja rzeczywistej losowości.

## Jak to zrobić:

Najczęstszym sposobem generowania losowych liczb w C# jest użycie klasy `System.Random`. Oto prosty przykład demonstrujący jej użycie:

```C#
using System;

public class RandomNumberExample
{
    static void Main(string[] args)
    {
        Random random = new Random();
        int randomNumber = random.Next(1, 100); // Generuje liczbę między 1 a 99
        Console.WriteLine($"Losowa liczba: {randomNumber}");
    }
}
```

Wygeneruje to liczbę losową, na przykład:

```
Losowa liczba: 42
```

Do generowania losowej liczby zmiennoprzecinkowej między 0,0 a 1,0, można użyć metody `NextDouble`:

```C#
double randomDouble = random.NextDouble();
Console.WriteLine($"Losowy double: {randomDouble}");
```

Jeśli pracujesz nad aplikacją wrażliwą na bezpieczeństwo, która wymaga kryptograficznej losowości, lepiej jest użyć klasy `RNGCryptoServiceProvider` znajdującej się w `System.Security.Cryptography`:

```C#
using System;
using System.Security.Cryptography;

public class SecureRandomExample
{
    static void Main()
    {
        byte[] randomNumber = new byte[4]; // Tworzy 4-bajtową długą liczbę losową
        using (RNGCryptoServiceProvider rng = new RNGCryptoServiceProvider())
        {
            rng.GetBytes(randomNumber);
        }
        int value = BitConverter.ToInt32(randomNumber, 0);
        Console.WriteLine($"Kryptograficznie bezpieczna liczba losowa: {value}");
    }
}
```

## Dogłębna analiza

Generowanie losowych liczb w C# ewoluowało na przestrzeni lat. Początkowo klasa `System.Random` była używana do generowania pseudo-losowych liczb. Jest to pseudo-losowe, ponieważ, biorąc pod uwagę konkretną wartość nasiona, wyprodukuje ten sam ciąg liczb, co może być użyteczne do debugowania lub powtarzalności testów.

Mimo że jest wystarczająca dla podstawowych potrzeb, `System.Random` nie jest bezpieczna w użyciu równoległym i może produkować przewidywalne wyniki, co nie jest odpowiednie dla aplikacji zależnych od bezpieczeństwa. Ta ograniczenie doprowadziło do wprowadzenia `RNGCryptoServiceProvider` dla kryptograficznej losowości, która jest bardziej bezpieczna, ale również bardziej zasobożerna.

Alternatywą w .NET Core i .NET 5+ jest klasa `RandomNumberGenerator` w `System.Security.Cryptography` do bezpiecznego generowania losowych liczb, która jest przeznaczona jako bardziej nowoczesna i łatwa w użyciu opcja w porównaniu do `RNGCryptoServiceProvider`.

Każda metoda generowania losowych liczb w C# ma swoje miejsce w zależności od wymagań aplikacji. Dla większości aplikacji wystarcza `System.Random`, ale dla tych, które wymagają bezpiecznych, nieprzewidywalnych losowych liczb, klasy kryptograficzne stanowią solidną alternatywę.
