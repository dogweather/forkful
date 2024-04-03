---
changelog:
- 2024-02-25, gpt-4-0125-preview, translated from English
date: 2024-02-25 17:06:54.235897-07:00
description: "Jak to zrobi\u0107: W C#, interpolacja ci\u0105gu jest oznaczana znakiem\
  \ dolara (`$`) po kt\xF3rym nast\u0119puje litera\u0142 ci\u0105gu. Nazwy zmiennych\
  \ lub wyra\u017Cenia umieszcza si\u0119\u2026"
lastmod: '2024-03-13T22:44:35.393600-06:00'
model: gpt-4-0125-preview
summary: "W C#, interpolacja ci\u0105gu jest oznaczana znakiem dolara (`$`) po kt\xF3\
  rym nast\u0119puje litera\u0142 ci\u0105gu."
title: "Interpolacja \u0142a\u0144cucha znak\xF3w"
weight: 8
---

## Jak to zrobić:
W C#, interpolacja ciągu jest oznaczana znakiem dolara (`$`) po którym następuje literał ciągu. Nazwy zmiennych lub wyrażenia umieszcza się w nawiasach klamrowych (`{}`).

```csharp
string name = "Jane";
int age = 28;
string interpolatedString = $"Cześć, {name}! Masz {age} lat.";
Console.WriteLine(interpolatedString);
// Output: Cześć, Jane! Masz 28 lat.
```

W bardziej złożonym przykładzie możesz wykonywać operacje lub wywoływać metody wewnątrz nawiasów klamrowych:

```csharp
double price = 19.99;
int quantity = 3;
string orderDetail = $"Całkowita cena: {price * quantity:C2}";
Console.WriteLine(orderDetail);
// Output: Całkowita cena: 59,97 $
```
Specyfikator formatu `:C2` wewnątrz nawiasów klamrowych formatuje liczbę jako walutę z dwoma miejscami po przecinku.

W scenariuszach wymagających bardziej zaawansowanego formatowania lub lokalizacji, możesz rozważyć użycie metody `string.Format` lub bibliotek takich jak Humanizer. Humanizer pozwala manipulować i wyświetlać ciągi, daty, czasy, okresy, liczby i ilości w bardziej zrozumiałym formacie. Poniżej znajduje się przykład użycia Humanizera do skomplikowanej manipulacji ciągiem. Zwróć uwagę, że Humanizer nie jest częścią standardowej biblioteki .NET i wymaga zainstalowania pakietu NuGet `Humanizer`.

Najpierw zainstaluj Humanizer przez NuGet:

```
Install-Package Humanizer
```

Następnie możesz użyć go w następujący sposób:

```csharp
using Humanizer;

int dayDifference = 5;
string humanized = $"Wydarzenie miało miejsce {dayDifference} dni temu.".Humanize();
Console.WriteLine(humanized);
// W zależności od konfiguracji i kultury, możliwy output: Wydarzenie miało miejsce 5 dni temu.
```

Ten przykład demonstruje podstawowe użycie. Humanizer wspiera szeroki zakres funkcjonalności, które mogą być stosowane do ciągów, dat, liczb i więcej, czyniąc twoje aplikacje bardziej dostępnymi i intuicyjnymi.
