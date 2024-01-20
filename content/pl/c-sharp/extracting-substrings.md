---
title:                "Wydobywanie podciągów"
html_title:           "Python: Wydobywanie podciągów"
simple_title:         "Wydobywanie podciągów"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/extracting-substrings.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Wyodrębnienie podciągów to proces, w którym wybierasz mniejszą cześć ciągu znaków, nazywaną podciągiem. Programiści robią to, by manipulować wybranymi danymi, zamiast całego ciągu.

## Jak to zrobić:

Metoda Substring w języku C# jest dość prosta w użyciu. Możemy z niej skorzystać w następujący sposób:

```C#
string str = "Cześć Wszystkim!";
string subStr = str.Substring(0, 7);
Console.WriteLine(subStr);
```

Wynik:

```C#
>Cześć W
```

Podajemy indeks i (opcjonalnie) ilość znaków, które chcemy uzyskać. W tym przypadku, zaczynamy od 0 (od początku ciągu), a następnie wybieramy 7 znaków.

## Pogłębiona analiza:

Metoda Substring istnieje już od pierwszych wydań C#. Mimo że przez ten czas pojawiły się alternatywy, takie jak metoda Slice (w C# 8.0), Substring nadal jest stosowany ze względu na swoją prostotę.

Przy skorzystaniu z metody Slice:

```C#
string str = "Cześć Wszystkim!";
ReadOnlySpan<char> slice = str.AsSpan().Slice(0, 7);
Console.WriteLine(slice.ToString());
```

Pamiętaj jedno - zarówno Substring jak i Slice tworzą nowy ciąg. Nie zmieniają one oryginalnego ciągu - w C# ciągi są niemutowalne.

## Zobacz też:

- Dokumentacja [Substring](https://docs.microsoft.com/pl-pl/dotnet/api/system.string.substring?view=net-5.0)
- Dokumentacja [Slice](https://docs.microsoft.com/pl-pl/dotnet/api/system.memoryextensions.slice?view=net-5.0)
- Artykuł [Ciągi w C#](https://docs.microsoft.com/pl-pl/dotnet/csharp/programming-guide/strings/)