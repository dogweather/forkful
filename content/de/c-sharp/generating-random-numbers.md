---
title:                "Zufallszahlen generieren"
html_title:           "Arduino: Zufallszahlen generieren"
simple_title:         "Zufallszahlen generieren"
programming_language: "C#"
category:             "C#"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Was & Warum?
Generieren von Zufallszahlen ist das Erzeugen von Zahlen, die keine erkennbare Muster oder Beziehung aufweisen. Entwickler nutzen sie oft für verschiedene Zwecke wie Algorithmen für Computerspiele.

## So geht's:
Das Erzeugen einer Zufallszahl in C# ist einfach und direkt. Man benötigt die `Random` Klasse. Ein einmaliges Codebeispiel ist unten dargestellt:
```C#
using System;

class Program
{
    static void Main()
    {
        Random zufallszahl = new Random();
        Console.WriteLine(zufallszahl.Next());  // Gibt eine zufällige Zahl aus.
    }
}
```

## Vertiefung:
Historisch gesehen verwendet die `Random` Klasse in .NET einen modifizierten Park-Miller Algorithmus für das Generieren seiner Pseudozufallszahlen. Es gibt alternative Möglichkeiten, wie die Verwendung des `RNGCryptoServiceProvider` für stärkere und sicherere Zufallszahlen, obwohl er langsamer als `Random` ist. Beachten Sie auch, dass die `Random` Klasse in C# nicht threadsicher ist und daher eine entsprechende Implementierung benötigt, wenn in mehreren Threads verwendet werden soll.

## Siehe auch:
Weitere Informationen finden Sie auf den unten genannten Seiten:
- [Microsoft Docs - Random Klasse](https://docs.microsoft.com/de-de/dotnet/api/system.random?view=net-5.0)
- [Microsoft Docs - RNGCryptoServiceProvider Klasse](https://docs.microsoft.com/de-de/dotnet/api/system.security.cryptography.rngcryptoserviceprovider)
- [Microsoft Docs - Parallele Programmierung in .NET](https://docs.microsoft.com/de-de/dotnet/standard/parallel-programming/)