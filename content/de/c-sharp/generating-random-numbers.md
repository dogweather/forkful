---
title:    "C#: : Generierung von Zufallszahlen"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Warum

Generieren von Zufallszahlen ist eine häufige Aufgabe in der Programmierung. Mit Hilfe von Zufallszahlen können wir Spiele entwerfen, Verschlüsselungsalgorithmen testen und vieles mehr. In dieser Blog-Post werden wir uns damit beschäftigen, wie man in C# Zufallszahlen generieren kann.

## Wie geht das?

Um Zufallszahlen in C# zu generieren, verwenden wir die Klasse `Random`. In unserem Beispiel generieren wir eine Zufallszahl zwischen 1 und 10 und geben sie auf der Konsole aus.

```C#
Random rand = new Random();
int number = rand.Next(1, 11);
Console.WriteLine("Die Zufallszahl ist: " + number);
```

Die Ausgabe sieht dann zum Beispiel so aus: `Die Zufallszahl ist: 5`. Jetzt können wir diese Zufallszahl für verschiedene Anwendungen nutzen.

Um eine Liste von Zufallszahlen zu generieren, können wir eine Schleife verwenden. In diesem Beispiel erstellen wir eine Liste mit 10 Zufallszahlen zwischen 1 und 100 und geben sie auf der Konsole aus.

```C#
List<int> randomNumberList = new List<int>();

for (int i = 0; i < 10; i++)
{
    int number = rand.Next(1, 101);
    randomNumberList.Add(number);
}

Console.WriteLine("Die Liste der Zufallszahlen:");

foreach (int number in randomNumberList)
{
    Console.Write(number + " ");
}

```

Die Ausgabe sieht dann zum Beispiel so aus: `Die Liste der Zufallszahlen: 25 62 7 41 95 12 87 3 52 17` 

## Tiefergehende Informationen

Die Klasse `Random` verwendet einen Pseudorandom-Algorithmus, um Zufallszahlen zu generieren. Das bedeutet, dass die Zahlen nicht wirklich zufällig sind, sondern auf einem vorhersehbaren Muster basieren. Wenn Sie eine Liste von Zufallszahlen benötigen, die wirklich zufällig sind, können Sie den Seed-Wert für die Klasse `Random` angeben. Ein Seed-Wert ist eine Startzahl für den Algorithmus und sorgt dafür, dass er für jede Anwendung verschiedene Zahlen generiert.

```C#
Random rand = new Random(123); // Seed-Wert von 123
```

Sie sollten auch daran denken, dass die generierten Zufallszahlen nicht wirklich zufällig sind und deshalb keine sensiblen Informationen, wie beispielsweise Passwörter oder Kreditkartennummern, darin ablegen sollten.

## Siehe auch

- [MSDN-Dokumentation über die Klasse `Random`](https://docs.microsoft.com/de-de/dotnet/api/system.random?view=netframework-4.8)
- [C# Zufallszahlen Tutorial von TutsPlus](https://code.tutsplus.com/tutorials/generating-random-numbers-in-net-a-practical-guide--cms-21387)
- [Wikipedia-Artikel über Zufallszahlen](https://de.wikipedia.org/wiki/Zufallszahlen)