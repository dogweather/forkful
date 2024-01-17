---
title:                "Erzeugung zufälliger Zahlen"
html_title:           "C#: Erzeugung zufälliger Zahlen"
simple_title:         "Erzeugung zufälliger Zahlen"
programming_language: "C#"
category:             "C#"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Generieren von Zufallszahlen ist ein grundlegender Bestandteil der Programmierung. Es bezieht sich auf den Prozess, bei dem zufällige Zahlen innerhalb eines bestimmten Bereichs erzeugt werden. Programmierer nutzen dies für verschiedene Zwecke, wie zum Beispiel für die Erstellung von Passwörtern oder die Simulation von zufälligen Ereignissen.

## Wie geht das?
In C# können wir die Klasse ```Random``` verwenden, um Zufallszahlen zu generieren. Zuerst müssen wir jedoch eine Instanz dieser Klasse erstellen und einen Seed-Wert übergeben. Der Seed-Wert dient als Startpunkt für die Berechnung der Zufallszahlen. Wenn wir keinen Seed-Wert angeben, wird standardmäßig die aktuelle Systemzeit verwendet.

```
Random random = new Random(); // Erstelle eine Instanz der Random Klasse
int number = random.Next(1, 10); // Generiere eine zufällige ganze Zahl zwischen 1 und 10
Console.WriteLine(number); // Gib die generierte Zahl aus
```

Einige gängige Methoden der Random Klasse sind ```Next()```, ```NextDouble()``` und ```NextBytes()```, die jeweils verschiedene Typen von Zufallszahlen generieren. Wir können auch die Methode ```Next(min, max)``` verwenden, um eine Zufallszahl innerhalb einer bestimmten Bereichsgrenze zu generieren.

## Tiefer Einblick
Das Generieren von Zufallszahlen hat eine lange Geschichte in der Mathematik und ist auch in der Informatik von großer Bedeutung. Früher wurden Zufallszahlen durch komplexe mathematische Algorithmen erzeugt, während heutzutage durch die Fortschritte in der Technologie schnellere und zuverlässigere Methoden entwickelt wurden.

Alternativ zur Verwendung der Random Klasse können Programmierer auch externe Bibliotheken wie die Mersenne Twister-Engine nutzen, die eine höhere Performance und mehr Kontrolle über die generierten Zufallszahlen bieten.

Die Random Klasse in C# implementiert den Park-Miller-Algorithmus, der auch als "Lehmer RNG" (Linearer Kongruenzgenerator) bekannt ist. Dieser Algorithmus nutzt einfache mathematische Berechnungen, um effizient Zufallszahlen zu generieren.

## Siehe auch
Weitere Informationen und Beispiele zur Verwendung der Random Klasse in C# finden Sie in der offiziellen Microsoft-Dokumentation: https://docs.microsoft.com/en-us/dotnet/api/system.random?view=netcore-3.1

Eine Einführung in die Konzepte und Methoden des Generierens von Zufallszahlen finden Sie hier: https://www.geeksforgeeks.org/random-number-generator-in-c-sharp/