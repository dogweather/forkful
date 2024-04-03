---
date: 2024-01-27 20:32:57.551325-07:00
description: "Das Generieren von Zufallszahlen in C# umfasst die Erstellung von unvorhersehbaren\
  \ numerischen Werten innerhalb eines spezifizierten Bereichs.\u2026"
lastmod: '2024-03-13T22:44:53.884014-06:00'
model: gpt-4-0125-preview
summary: Das Generieren von Zufallszahlen in C# umfasst die Erstellung von unvorhersehbaren
  numerischen Werten innerhalb eines spezifizierten Bereichs.
title: Generierung von Zufallszahlen
weight: 12
---

## Was & Warum?

Das Generieren von Zufallszahlen in C# umfasst die Erstellung von unvorhersehbaren numerischen Werten innerhalb eines spezifizierten Bereichs. Programmierer verwenden diese Methoden, um Funktionen wie Kryptographie, Simulationen und Spiele zu implementieren, bei denen Unvorhersehbarkeit oder die Simulation von realweltlicher Zufälligkeit erforderlich ist.

## Wie geht das:

Die gebräuchlichste Methode, um Zufallszahlen in C# zu generieren, ist die Verwendung der `System.Random` Klasse. Hier ist ein einfaches Beispiel, das deren Nutzung demonstriert:

```C#
using System;

public class RandomNumberExample
{
    static void Main(string[] args)
    {
        Random random = new Random();
        int randomNumber = random.Next(1, 100); // Generiert eine Zahl zwischen 1 und 99
        Console.WriteLine($"Zufallszahl: {randomNumber}");
    }
}
```

Dies wird eine zufällige Zahl ausgeben wie:

```
Zufallszahl: 42
```

Um eine zufällige Gleitkommazahl zwischen 0,0 und 1,0 zu generieren, können Sie die Methode `NextDouble` verwenden:

```C#
double randomDouble = random.NextDouble();
Console.WriteLine($"Zufälliger Double-Wert: {randomDouble}");
```

Wenn Sie an einer sicherheitskritischen Anwendung arbeiten, die kryptographische Zufälligkeit erfordert, ist es besser, die `RNGCryptoServiceProvider` Klasse aus `System.Security.Cryptography` zu verwenden:

```C#
using System;
using System.Security.Cryptography;

public class SecureRandomExample
{
    static void Main()
    {
        byte[] randomNumber = new byte[4]; // Erstellt eine 4 Byte lange Zufallszahl
        using (RNGCryptoServiceProvider rng = new RNGCryptoServiceProvider())
        {
            rng.GetBytes(randomNumber);
        }
        int value = BitConverter.ToInt32(randomNumber, 0);
        Console.WriteLine($"Kryptographisch sichere Zufallszahl: {value}");
    }
}
```

## Vertiefung

Die Generierung von Zufallszahlen in C# hat sich im Laufe der Jahre weiterentwickelt. Ursprünglich war die `System.Random` Klasse der Standard für die Erzeugung von Pseudozufallszahlen. Es ist pseudozufällig, weil es, gegeben einen spezifischen Seed-Wert, die gleiche Zahlenfolge produzieren wird, was für das Debuggen oder die Wiederholbarkeit von Tests nützlich sein kann.

Obwohl sie für grundlegende Bedürfnisse ausreichend ist, ist `System.Random` nicht threadsicher und kann vorhersehbare Ergebnisse liefern, was für sicherheitsabhängige Anwendungen nicht geeignet ist. Diese Einschränkung führte zur Einführung des `RNGCryptoServiceProvider` für kryptographische Zufälligkeit, der sicherer, aber auch ressourcenintensiver ist.

Eine Alternative in .NET Core und .NET 5+ ist die `RandomNumberGenerator` Klasse in `System.Security.Cryptography` zur sicheren Generierung von Zufallszahlen, die als eine modernere und benutzerfreundlichere Option im Vergleich zu `RNGCryptoServiceProvider` gedacht ist.

Jede Methode zur Generierung von Zufallszahlen in C# hat ihren Platz, abhängig von den Anforderungen der Anwendung. Für die meisten Anwendungen reicht `System.Random` aus, aber für diejenigen, die sichere, unvorhersehbare Zufallszahlen erfordern, bieten die kryptographischen Klassen eine robuste Alternative.
