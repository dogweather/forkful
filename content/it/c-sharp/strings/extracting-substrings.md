---
date: 2024-01-20 17:45:37.947278-07:00
description: 'How to: (Come fare:) Utilizza `Substring`, `Span<T>`, `String.Split`,
  `^` e `..` per estrarre sottostringhe. Ecco degli esempi.'
lastmod: '2024-04-05T21:53:44.192048-06:00'
model: gpt-4-1106-preview
summary: (Come fare:) Utilizza `Substring`, `Span<T>`, `String.Split`, `^` e `..`
  per estrarre sottostringhe.
title: Estrazione di sottostringhe
weight: 6
---

## How to:
(Come fare:)
Utilizza `Substring`, `Span<T>`, `String.Split`, `^` e `..` per estrarre sottostringhe. Ecco degli esempi:

```C#
string saluto = "Ciao a tutti da C#";

// Con Substring
string parola1 = saluto.Substring(0, 4);
Console.WriteLine(parola1); // Output: Ciao

// Con Span<T> per sottostringhe su grandi testi senza allocazioni extra
ReadOnlySpan<char> spanSaluto = saluto.AsSpan();
ReadOnlySpan<char> spanParola = spanSaluto[0..4];
Console.WriteLine(spanParola.ToString()); // Output: Ciao

// Con String.Split
string[] parole = saluto.Split(' ');
Console.WriteLine(parole[0]); // Output: Ciao

// Con ^ e ..
string salutoDaEnd = saluto[^2..];
Console.WriteLine(salutoDaEnd); // Output: # (ultimo carattere)
```

## Deep Dive:
(Analisi Approfondita:)
Estrarre sottostringhe è un must fin dai primi linguaggi di programmazione. In C#, `Substring` è stato il metodo classico fin dall'inizio. Dal C# 8.0, con la introduzione di `Span<T>` e le espressioni di intervallo (`..`), si gestiscono meglio le prestazioni evitando la creazione di nuove stringhe. Prima di `Span<T>`, operazioni simili potevano essere costose in termini di memoria, specialmente con testi grandi.

## See Also:
(Vedi anche:)
- Documentazione ufficiale su `Substring`: [Microsoft Docs - Substring](https://docs.microsoft.com/en-us/dotnet/api/system.string.substring)
- Articolo su `Span<T>`: [Microsoft Docs - Span](https://docs.microsoft.com/en-us/dotnet/api/system.span-1)
- Spiegazione sugli intervalli in C#: [Microsoft Docs - Range operator](https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/operators/member-access-operators#range-operator-)
- Guida su `String.Split`: [Microsoft Docs - String.Split](https://docs.microsoft.com/en-us/dotnet/api/system.string.split)
