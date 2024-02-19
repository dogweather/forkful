---
aliases:
- /pl/c-sharp/interpolating-a-string/
date: 2024-01-20 17:50:40.594514-07:00
description: "Interpolacja string\xF3w to spos\xF3b wkluczania zmiennych bezpo\u015B\
  rednio w ci\u0105gi tekstowe. Programi\u015Bci u\u017Cywaj\u0105 jej, by \u0142\
  atwo \u0142\u0105czy\u0107 tekst z danymi, co czyni kod\u2026"
lastmod: 2024-02-18 23:08:49.597295
model: gpt-4-1106-preview
summary: "Interpolacja string\xF3w to spos\xF3b wkluczania zmiennych bezpo\u015Brednio\
  \ w ci\u0105gi tekstowe. Programi\u015Bci u\u017Cywaj\u0105 jej, by \u0142atwo \u0142\
  \u0105czy\u0107 tekst z danymi, co czyni kod\u2026"
title: "Interpolacja \u0142a\u0144cuch\xF3w znak\xF3w"
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
