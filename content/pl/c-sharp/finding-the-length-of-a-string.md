---
title:    "C#: Odnajdywanie długości ciągu znaków"
keywords: ["C#"]
---

{{< edit_this_page >}}

## Dlaczego

Długość ciągu znaków jest jedną z podstawowych operacji w programowaniu. Znajomość tej funkcji jest niezbędna do prawidłowej manipulacji tekstami w C#.

## Jak to zrobić

Obliczenie długości ciągu znaków w C# jest proste i wymaga użycia metody `Length` na zmiennej typu `string`. Przykładowy kod wraz z oczekiwanym wynikiem przedstawiono poniżej:

```C#
string name = "Karolina";
int length = name.Length;
Console.WriteLine(length); // Output: 8
```

Powyższy przykład pokazuje, że długość ciągu znaków obejmuje wszystkie znaki, w tym spacje i znaki specjalne.

## Wnikliwe spojrzenie

Długość ciągu znaków może być używana do wielu różnych celów w programowaniu. Może służyć do walidacji długości wprowadzonego przez użytkownika hasła lub imienia, a także do obliczania ilości znaków w plikach tekstowych. W przypadku konieczności pracy z dłuższymi tekstami, może być wykorzystana w pętlach do podziału tekstu na mniejsze fragmenty.

## Zobacz także
- [Dokumentacja Microsoft dotycząca długości ciągów znaków w C#](https://docs.microsoft.com/en-us/dotnet/api/system.string.length?view=netcore-3.1)
- [Przydatne porady dla początkujących programistów C#](https://blog.codersclan.net/pl/powtorka-byc-programista-csharp/)
- [Rozwiązania dla trudności związanych z manipulacją ciągami znaków](https://davidwalsh.name/string-from-file)