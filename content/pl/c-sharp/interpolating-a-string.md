---
title:                "Interpolacja łańcuchów znaków"
date:                  2024-01-20T17:50:40.594514-07:00
model:                 gpt-4-1106-preview
simple_title:         "Interpolacja łańcuchów znaków"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/c-sharp/interpolating-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (Co i Dlaczego?)

Interpolacja stringów to sposób wkluczania zmiennych bezpośrednio w ciągi tekstowe. Programiści używają jej, by łatwo łączyć tekst z danymi, co czyni kod bardziej czytelnym i prostszym w utrzymaniu.

## How to (Jak to zrobić):

Używamy znaku `$` i nawiasów klamrowych `{}`. Prosto, szybko, wygodnie.

```C#
string name = "Ania";
int age = 25;
string greeting = $"Cześć, nazywam się {name} i mam {age} lata.";
Console.WriteLine(greeting);
```

Output:
```
Cześć, nazywam się Ania i mam 25 lata.
```

Możesz nawet wykonywać proste operacje:
```C#
int a = 10;
int b = 5;
string math = $"Dziesięć minus pięć to {a - b}.";
Console.WriteLine(math);
```

Output:
```
Dziesięć minus pięć to 5.
```

## Deep Dive (Głębsze spojrzenie)

Interpolacja stringów w C# została wprowadzona w wersji 6.0. To ewolucja starego `String.Format()`, znacznie poprawiająca czytelność. Alternatywą są konkatenacja (+) i `StringBuilder`, ale one często sprawiają, że kod jest mniej zrozumiały.

Implementacja w .NET używa `String.Format`, a za kulisami przekształca interpolowany string w odpowiedni kod wykorzystujący tę funkcję. Warto pamiętać, że interpolowane stringi są typami referencyjnymi i tworzą obiekty w trakcie wykonania programu.

## See Also (Zobacz też)

- [Dokumentacja Microsoft o interpolacji w C#](https://docs.microsoft.com/pl-pl/dotnet/csharp/language-reference/tokens/interpolated)
- [String.Format w MSDN](https://docs.microsoft.com/pl-pl/dotnet/api/system.string.format?view=net-6.0)
- [StringBuilder w MSDN](https://docs.microsoft.com/pl-pl/dotnet/api/system.text.stringbuilder?view=net-6.0)
- [Tutorial o konkatenacji stringów](https://docs.microsoft.com/pl-pl/dotnet/csharp/programming-guide/strings/)
