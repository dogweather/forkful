---
date: 2024-01-20 17:34:15.756437-07:00
description: "Jak to zrobi\u0107: Konkatenacj\u0119 wykorzystujemy od samego pocz\u0105\
  tku C#. Alternatywy? `String.Concat()`, `String.Format()` czy interpolacja, wprowadzona\
  \ w C# 6.0\u2026"
lastmod: '2024-04-05T21:53:36.835604-06:00'
model: gpt-4-1106-preview
summary: "Konkatenacj\u0119 wykorzystujemy od samego pocz\u0105tku C#."
title: "\u0141\u0105czenie \u0142a\u0144cuch\xF3w znak\xF3w"
weight: 3
---

## Jak to zrobić:
```C#
string firstName = "Jan";
string lastName = "Kowalski";
string fullName = firstName + " " + lastName; // Konkatenacja za pomocą operatora +

Console.WriteLine(fullName);
// Wyjście: Jan Kowalski

// Użycie StringBuilder dla efektywności przy wielokrotnym doklejaniu
var builder = new StringBuilder();
builder.Append("Cześć, ");
builder.Append(firstName);
builder.Append(" ");
builder.Append(lastName);
builder.Append("!");

Console.WriteLine(builder.ToString());
// Wyjście: Cześć, Jan Kowalski!
```

## Wgłębiamy się
Konkatenację wykorzystujemy od samego początku C#. Alternatywy? `String.Concat()`, `String.Format()` czy interpolacja, wprowadzona w C# 6.0 z użyciem `$""`. Interpolacja jest czytelna i szybka. `StringBuilder` jest idealny przy dużych czy częstych operacjach na łańcuchach, bo nie tworzy nowego stringa za każdym razem.

## Zobacz również
- Dokumentacja Microsoft o konkatenacji łańcuchów: [docs.microsoft.com](https://docs.microsoft.com/en-us/dotnet/csharp/how-to/concatenate-multiple-strings)
- Interpolacja łańcuchów z C#: [docs.microsoft.com](https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/tokens/interpolated)
- Klasa StringBuilder: [docs.microsoft.com](https://docs.microsoft.com/en-us/dotnet/api/system.text.stringbuilder?view=net-6.0)
