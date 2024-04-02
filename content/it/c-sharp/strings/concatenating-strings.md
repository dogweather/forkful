---
date: 2024-01-20 17:34:42.744165-07:00
description: "Concatenare le stringhe significa unirle in una sola. I programmatori\
  \ lo fanno per costruire testo dinamicamente, come combinare nome e cognome o generare\u2026"
lastmod: '2024-03-13T22:44:43.426200-06:00'
model: gpt-4-1106-preview
summary: "Concatenare le stringhe significa unirle in una sola. I programmatori lo\
  \ fanno per costruire testo dinamicamente, come combinare nome e cognome o generare\u2026"
title: Concatenazione di stringhe
weight: 3
---

## What & Why? (Cosa e Perché?)
Concatenare le stringhe significa unirle in una sola. I programmatori lo fanno per costruire testo dinamicamente, come combinare nome e cognome o generare messaggi informativi.

## How to (Come Fare)
Ecco alcuni modi per concatenare stringhe in C#:

```C#
string firstname = "Mario";
string lastname = "Rossi";
string fullname = firstname + " " + lastname; // Concatenazione con l'operatore +
Console.WriteLine(fullname); // Output: Mario Rossi

string greeting = "Ciao, ";
greeting += firstname; // Concatenazione con operatore +=
Console.WriteLine(greeting); // Output: Ciao, Mario

string city = "Roma";
string welcome = String.Concat(greeting, " benvenuto a ", city, "!"); // Concatenazione con String.Concat
Console.WriteLine(welcome); // Output: Ciao, Mario benvenuto a Roma!

string info = $"Il tuo nome completo è {fullname} e vivi a {city}."; // Interpolazione di stringa
Console.WriteLine(info); // Output: Il tuo nome completo è Mario Rossi e vivi a Roma.
```

## Deep Dive (Analisi Approfondita)
Un tempo, in C#, concatenare stringhe con l'operatore `+` in un ciclo poteva essere dispendioso in termini di prestazioni. Lo era perché creava un nuovo oggetto stringa a ogni iterazione. Questo problema venne risolto con l'introduzione del tipo `StringBuilder`, che è più efficiente in scenari simili.

Alternative moderne includono l'interpolazione di stringa, introdotta in C# 6, che rende il codice più leggibile e manutenibile. È importante notare, però, che l'interpolazione usa internamente `String.Format`, che può essere meno efficiente di `StringBuilder` per grandi quantità di concatenazioni.

```C#
StringBuilder sb = new StringBuilder();
sb.Append("Ciao");
sb.Append(" ");
sb.Append("mondo");
string message = sb.ToString(); // Output: Ciao mondo
```

Implementazione dipende anche da come .NET gestisce le stringhe nella memoria: sono immutabili. Concatenazioni frequenti possono portare a un uso di memoria intensivo, quindi scegliereil metodo adeguato è fondamentale.

## See Also (Vedi Anche)
- [Microsoft - String.Concat Method](https://docs.microsoft.com/en-us/dotnet/api/system.string.concat?view=netcore-3.1)
- [Microsoft - StringBuilder Class](https://docs.microsoft.com/en-us/dotnet/api/system.text.stringbuilder?view=netcore-3.1)
- [Stack Overflow - When to use StringBuilder?](https://stackoverflow.com/questions/73883/when-to-use-stringbuilder)
- [Microsoft - String Interpolation in C#](https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/tokens/interpolated)
