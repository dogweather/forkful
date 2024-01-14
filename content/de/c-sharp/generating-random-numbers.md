---
title:                "C#: Zufallszahlen generieren"
programming_language: "C#"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/generating-random-numbers.md"
---

{{< edit_this_page >}}

### Warum
Generieren von Zufallszahlen ist ein häufiges Konzept in der Programmierung. Es kann für verschiedene Anwendungen wie Simulationen, Spiele oder Verschlüsselung verwendet werden. In diesem Blogbeitag werden wir uns mit der Generierung von Zufallszahlen in C# befassen und diskutieren, wie man dies effektiv umsetzen kann.

### Wie man es macht
Die Generierung von Zufallszahlen in C# ist relativ einfach. Zunächst müssen wir die Klasse "Random" importieren, die in der System-Standardbibliothek enthalten ist.

```C#
using System;
```

Als nächstes müssen wir eine Instanz der Klasse erstellen und eine Zufallszahl mit der Methode "Next()" generieren. Wir können die Methode mit oder ohne Parameter aufrufen. Wenn wir keine Parameter angeben, generiert sie eine positive Ganzzahl. Wenn wir eine Mindest- und eine Höchstgrenze angeben, generiert sie eine Zahl innerhalb dieses Bereichs.

```C#
Random random = new Random();
int randomNumber = random.Next();
int randomInRange = random.Next(1, 10);

Console.WriteLine("Eine Zufallszahl ohne Range: " + randomNumber);
Console.WriteLine("Eine Zufallszahl mit Range (1-10): " + randomInRange);
```

Die Ausgabe könnte wie folgt aussehen:

```
Eine Zufallszahl ohne Range: 584640345
Eine Zufallszahl mit Range (1-10): 7
```

Wir können auch Zufallszahlen mit anderen Datentypen generieren, indem wir die entsprechenden Methoden verwenden.

```C#
double randomDouble = random.NextDouble();
bool randomBool = Convert.ToBoolean(random.Next(0, 2));

Console.WriteLine("Eine Zufallszahl vom Typ Double: " + randomDouble);
Console.WriteLine("Eine Zufallszahl vom Typ Boolean: " + randomBool);
```

Die Ausgabe könnte wie folgt aussehen:

```
Eine Zufallszahl vom Typ Double: 0.427184669
Eine Zufallszahl vom Typ Boolean: False
```

### Tiefer eintauchen
Die Klasse "Random" verwendet einen sogenannten "Pseudo-Zufallsgenerator", der auf einer mathematischen Formel basiert. Dies bedeutet, dass die generierten Zahlen zwar willkürlich erscheinen, aber in Wirklichkeit vorhersagbar sind.

Um dieses Problem zu umgehen, können wir den Startwert des Generators mit der Methode "Next(int seed)" setzen. Dadurch wird die generierte Zufallszahl von diesem Startwert aus berechnet, was eine größere Zufälligkeit ermöglicht.

```C#
Random random = new Random();
int randomNumber = random.Next();
Console.WriteLine("Eine Zufallszahl ohne Seed: " + randomNumber);

Random randomWithSeed = new Random(12345);
int randomNumberWithSeed = randomWithSeed.Next();
Console.WriteLine("Eine Zufallszahl mit Seed (12345): " + randomNumberWithSeed);
```

Die Ausgabe könnte wie folgt aussehen:

```
Eine Zufallszahl ohne Seed: 1929196294
Eine Zufallszahl mit Seed (12345): 557231888
```

Es gibt auch verschiedene Methoden und Techniken, die wir verwenden können, um bessere Zufallszahlen zu generieren, beispielsweise durch Verwendung von externen Ressourcen wie Hardware-Entropiequellen oder durch Verwendung von Kryptografie-Bibliotheken.

### Siehe auch
- [Microsoft Dokumentation - Generieren von Zufallszahlen in C# (in deutscher Sprache)](https://docs.microsoft.com/de-de/dotnet/api/system.random)
- [Artikel von C# Corner - Generieren von Zufallszahlen in C#](https://www.c-sharpcorner.com/article/generating-random-number-and-string-in-C-Sharp/) 
- [Blogbeitrag von Codeburst -8 Möglichkeiten, Zufallszahlen in C# zu generieren](https://codeburst.io/8-ways-to-generate-random-numbers-in-C-sharp-7e0621744561)