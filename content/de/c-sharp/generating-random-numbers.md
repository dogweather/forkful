---
title:    "C#: Erzeugung zufälliger Zahlen"
keywords: ["C#"]
---

{{< edit_this_page >}}

## Warum

Die Generierung von Zufallszahlen ist ein wichtiger Bestandteil der Programmierung und kann in verschiedenen Anwendungsfällen nützlich sein. Zum Beispiel können Zufallszahlen verwendet werden, um ein Spiel zu erstellen, kryptografische Schlüssel zu generieren oder auch für statistische Simulationen in der Forschung.

## Wie

Um in C# eine Zufallszahl zu generieren, gibt es mehrere Möglichkeiten. Zunächst müssen Sie die `Random`-Klasse importieren. Dann können Sie entweder die `Next()`-Methode verwenden, um eine ganze Zahl zurückzugeben, oder die `NextDouble()`-Methode, um eine Fließkommazahl zwischen 0 und 1 zurückzugeben.

```C#
using System;

Random rand = new Random();

// Ganze Zahl zwischen 0 und 10 generieren
int randomNumber = rand.Next(0, 11);

// Fließkommazahl zwischen 0 und 1 generieren
double randomDouble = rand.NextDouble();
```

Sie können auch eine Zufallszahl mit bestimmten Parametern generieren, zum Beispiel eine ganze Zahl zwischen 1 und 100 oder eine Fließkommazahl zwischen 5 und 10.

```C#
// Ganze Zahl zwischen 1 und 100 generieren
int randomNumber = rand.Next(1, 101);

// Fließkommazahl zwischen 5 und 10 generieren
double randomDouble = rand.NextDouble() * (10 - 5) + 5;
```

## Deep Dive

Die `Random`-Klasse verwendet einen sogenannten "Pseudozufallsgenerator", der eine vorhersehbare Sequenz von Zufallszahlen basierend auf einem "Seed" (Startwert) generiert. Dieser Seed kann entweder automatisch mit Hilfe der Systemzeit oder manuell durch den Entwickler festgelegt werden.

Es ist jedoch wichtig zu beachten, dass diese Zufallszahlen nicht wirklich zufällig sind, sondern auf einer mathematischen Berechnung basieren. Daher sollte dieser Generator nicht für sicherheitsrelevante Anwendungen wie Verschlüsselung verwendet werden.

## Siehe auch

- [Microsoft Dokumentation über die Random-Klasse](https://docs.microsoft.com/de-de/dotnet/api/system.random)
- [Artikel über die Unterschiede zwischen echten und Pseudozufallszahlen](https://www.statsoft.de/support/base/echte-und-pseudozufallszahlen/)
- [Beispielprojekt, das die Verwendung von Zufallszahlen in einem Spiel zeigt](https://github.com/udacity/2D-UFO)