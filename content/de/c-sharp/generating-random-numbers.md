---
title:                "Generierung von Zufallszahlen"
date:                  2024-01-20T17:48:50.459256-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generierung von Zufallszahlen"
programming_language: "C#"
category:             "C#"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Was & Warum?
Zufallszahlen sind Zahlen, die durch einen Prozess erzeugt werden, bei dem das Ergebnis nicht vorhersagbar ist. In der Programmierung nutzen wir sie für alles Mögliche, von Spieleentwicklung bis Sicherheitsfeatures.

## So geht's:
In C# verwenden wir die Klasse `Random` für einfache Zufallszahlen. Hier ein kurzes Beispiel:

```csharp
using System;

public class RandomNumbers
{
    public static void Main()
    {
        Random rand = new Random();

        Console.WriteLine(rand.Next()); // Gibt eine Zufallszahl aus
        Console.WriteLine(rand.Next(10, 50)); // Gibt eine Zufallszahl zwischen 10 und 50 aus
    }
}
```

Ausgabe könnte sein:
```
123456789
23
```

## Deep Dive:
Die Klasse `Random` in C# nutzt einen Pseudozufallszahlengenerator. Das bedeutet, dass die Zahlen nicht wirklich zufällig sind, sondern durch einen Algorithmus generiert werden, der vorhersagbar wird, wenn man den Startwert (den sogenannten Seed) kennt. Üblicherweise wird die aktuelle Zeit als Seed verwendet.

Für sicherheitskritische Anwendungen sollte `Random` vermieden werden. Stattdessen bietet .NET die `RNGCryptoServiceProvider` Klasse aus dem `System.Security.Cryptography` Namespace, die stärkere Zufälligkeit bietet.

Historisch gesehen entstanden die ersten Zufallszahlengeneratoren als Tabellen von Zahlen, die manuell zusammengestellt wurden, iterierten später über mechanische und dann über elektronische Geräte, bis hin zu modernen computergestützten Methoden.

Alternativen zu `Random` könnten die Nutzung von Drittanbieter-Bibliotheken sein, die auf verschiedenen Algorithmen basieren (z.B. Mersenne Twister), um unterschiedliche Bedürfnisse zu erfüllen.

## Siehe Auch:
- Microsoft-Dokumentation zur `Random` Klasse: [docs.microsoft.com](https://docs.microsoft.com/en-us/dotnet/api/system.random?view=net-7.0)
- Microsoft-Dokumentation für `RNGCryptoServiceProvider`: [docs.microsoft.com](https://docs.microsoft.com/en-us/dotnet/api/system.security.cryptography.rngcryptoserviceprovider?view=net-7.0)
- Einen ausführlicheren Leitfaden zum Thema "Zufälligkeit" in der Informatik: [Wikipedia - Pseudorandom number generator](https://en.wikipedia.org/wiki/Pseudorandom_number_generator)