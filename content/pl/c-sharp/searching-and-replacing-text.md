---
title:                "C#: Wyszukiwanie i zastępowanie tekstu"
simple_title:         "Wyszukiwanie i zastępowanie tekstu"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Dlaczego

Często w trakcie pisania programów, napotykamy sytuacje, w których musimy dokonać zmiany w tekście. Może to być aktualizacja nazw zmiennych lub poprawa błędów w tekście. Dlatego znajomość metod wyszukiwania i zamiany tekstu w języku C# jest niezbędna dla każdego programisty.

## Jak to zrobić

W języku C#, w celu wyszukiwania i zamiany tekstu, najczęściej wykorzystujemy funkcję .Replace(). Przyjmuje ona dwa parametry - pierwszy to ciąg znaków, który chcemy znaleźć, a drugi to ciąg znaków, który ma zostać wstawiony w jego miejsce. Przykładowo, jeśli chcemy zamienić słowo "hello" na "hi" w tekście "Hello, world!", nasz kod wyglądałby następująco:

```C#
string text = "Hello, world!";
text = text.Replace("Hello", "Hi");
Console.WriteLine(text); //Wynik: Hi, world!
```

Zwróćmy uwagę, że funkcja .Replace() jest metoda typu string, co oznacza, że zwraca ona nowy ciąg znaków, a nie dokonuje zmian w miejscu.

Inną przydatną metodą jest .ReplaceFirst(), dzięki której możemy zastosować zmianę tylko w pierwszym wystąpieniu danego wyrażenia. Możemy to osiągnąć poprzez użycie dodatkowego parametru, który określi, które wystąpienie ma być zmienione. Przykładowo, jeśli nasz tekst to "I am learning C#, C#, C#", a chcemy zamienić tylko pierwsze wystąpienie "C#" na "Java", nasz kod wyglądałby następująco:

```C#
string text = "I am learning C#, C#, C#";
text = text.ReplaceFirst("C#", "Java", 1);
Console.WriteLine(text); //Wynik: I am learning Java, C#, C#
```

## Deep Dive

Funkcja .Replace() wykorzystuje metodę IndexOf() do wyszukiwania ciągu znaków w tekście. Zwraca ona indeks pierwszego wystąpienia danego wyrażenia w tekście lub -1, jeśli wyrażenie nie zostanie znalezione. Dzięki temu, na podstawie indeksów, funkcja .Replace() wie, gdzie dokonać zmiany.

Jednym z zastosowań funkcji .Replace() może być również usuwanie fragmentów tekstu, poprzez podanie pustego ciągu znaków jako drugiego parametru. 

Dodatkowo, w języku C# istnieje wiele innych metod służących do wyszukiwania i zamiany tekstu, takich jak .ReplaceLast(), .ReplaceAll() czy .ReplaceRange(). Warto je poznać i zastosować w zależności od potrzeb.

## Zobacz też

- Dokumentacja C#, dział "String.Replace Method": https://docs.microsoft.com/en-us/dotnet/api/system.string.replace?view=netframework-4.8
- Przewodnik po języku C#: https://www.tutorialspoint.com/csharp/index.htm
- Wyszukiwanie i zamiana tekstu w języku C# - przydatne wskazówki i triki: https://www.c-sharpcorner.com/article/top-10-string-functions-in-c-sharp/