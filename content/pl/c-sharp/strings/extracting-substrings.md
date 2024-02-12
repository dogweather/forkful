---
title:                "Wycinanie podłańcuchów"
aliases:
- pl/c-sharp/extracting-substrings.md
date:                  2024-01-20T17:45:39.433286-07:00
model:                 gpt-4-1106-preview
simple_title:         "Wycinanie podłańcuchów"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why? (Co i dlaczego?)
Wycinanie podciągów to odizolowanie wybranych części z większego ciągu znaków. Robimy to, by manipulować danymi, wyciągać istotne informacje, czy sprawdzić strukturę tekstu.

## How to: (Jak to zrobić:)
```C#
using System;

class Program
{
    static void Main()
    {
        string fullText = "Cześć, jak się masz?";
        
        // Użyj 'Substring(startIndex, length)' do wycięcia "jak"
        string extracted = fullText.Substring(7, 3);
        Console.WriteLine(extracted);  // Output: jak

        // Użyj 'Substring(startIndex)' do wycięcia wszystkiego od "się"
        string remainingText = fullText.Substring(11);
        Console.WriteLine(remainingText);  // Output: się masz?
    }
}
```

## Deep Dive (Głębokie zanurzenie):
Wycinanie podciągów tekstowych nie zawsze było takie łatwe. Historia funkcji `Substring` w językach programowania pokazuje, jak wzrosło znaczenie prostoty i czytelności kodu. Alternatywnie, można użyć metod takich jak `Split`, `Remove`, `Replace`, czy LINQ, aby osiągnąć podobny efekt, ale z różnym stopniem kontroli i skomplikowania. W C#, `Substring` odwołuje się do znaków ciągu jako tablicy, ale z bezpieczeństwem typów i zarządzaniem pamięcią, tak, by operacje były zarówno wydajne, jak i bezpieczne.

## See Also (Zobacz także):
- [String.Substring Method in C# | Microsoft Docs](https://docs.microsoft.com/en-us/dotnet/api/system.string.substring)
- [Manipulating Strings in C# | C# Fundamentals by Scott Allen](https://app.pluralsight.com/library/courses/csharp-fundamentals-dev/table-of-contents)
- [System.String | MSDN](https://msdn.microsoft.com/en-us/library/system.string(v=vs.110).aspx)
