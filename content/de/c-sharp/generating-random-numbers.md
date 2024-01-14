---
title:                "C#: Zufallszahlen generieren"
simple_title:         "Zufallszahlen generieren"
programming_language: "C#"
category:             "C#"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Warum

Das Erstellen von Zufallszahlen ist eine wichtige Fähigkeit in der Programmierung. Es ermöglicht die Erstellung von einzigartigen und nicht vorhersehbaren Werten, die in vielen Anwendungsbereichen eingesetzt werden können, wie z.B. in der Spieleentwicklung, bei der Erstellung von Passwörtern oder bei der Durchführung von Tests.

## Wie man Zufallszahlen in C# erstellt

Um Zufallszahlen in C# zu erstellen, verwenden wir die Klasse Random aus dem Namespace System. Im folgenden Code-Beispiel erstellen wir eine Zufallszahl zwischen 1 und 100 und geben sie auf der Konsole aus:

```C#
using System;

Random rnd = new Random();
int number = rnd.Next(1, 101); // generiert eine Zufallszahl zwischen 1 und 100
Console.WriteLine(number); // Ausgabe auf der Konsole
```

Die Ausgabe könnte z.B. "49" sein.

## Tieferer Einblick

Bei der Erstellung von Zufallszahlen gibt es mehrere Dinge zu beachten. Die Ergebnisse sollten gleichmäßig verteilt sein und die Generierung sollte möglichst nicht vorhersehbar sein. In C# wird für die Erstellung von Zufallszahlen ein sogenannter Seed verwendet, der als Startwert für den Zufallszahlengenerator dient. Standardmäßig wird der aktuelle Zeitstempel als Seed verwendet, aber es ist auch möglich, einen bestimmten Seed manuell zu setzen. Dies kann z.B. für Testzwecke nützlich sein, um immer die gleiche Zufallszahl zu erhalten.

Weiterhin kann es sinnvoll sein, die Zufallszahlengenerierung in einem bestimmten Bereich zu begrenzen. Dazu können wir die `Next()` Methode mit Parametern verwenden, wie im oben genannten Beispiel, wo wir eine Zufallszahl zwischen 1 und 100 generiert haben. Es ist auch möglich, ganze Zahlen, Gleitkommazahlen und sogar Booleans mit der entsprechenden `Next()` Methode zu generieren.

## Siehe auch

- Dokumentation zur Random Klasse in C# (https://docs.microsoft.com/de-de/dotnet/api/system.random?view=net-5.0)
- Tutorial zum Erstellen von Zufallszahlen in C# (https://www.tutorialspoint.com/csharp/csharp_random_numbers.htm)
- Weitere Möglichkeiten zur Erstellung von Zufallszahlen in C# (https://www.c-sharpcorner.com/article/c-sharp-random-number-generate-using-random-class/)