---
title:                "Die Länge eines Strings ermitteln"
html_title:           "Java: Die Länge eines Strings ermitteln"
simple_title:         "Die Länge eines Strings ermitteln"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?

In der Programmiersprache C# bezeichnet die Länge eines Zeichenketten (Strings) die Anzahl der Zeichen in diesem String. Sie zu wissen, ist oft nützlich, zum Beispiel um Eingaben zu prüfen oder den Speicherbedarf vorauszuberechnen.

## So geht's:

Die Länge eines Strings ermitteln wir in C# mit der Eigenschaft `Length`. Hier ist die Syntax:

```C#
string meinString = "Hallo, Welt!";
int laenge = meinString.Length;
```

Die Ausgabe sieht so aus: 

```C#
Console.WriteLine(laenge);
// Ausgabe: 13
```

## Vertiefung

(1) Historisch: die Methode `.Length` ist in C# in der Klasse `String` schon seit der ersten Version vorhanden. Sie unterscheidet sich von `strlen` in C und `length` in Java nur in der Schreibweise.

(2) Alternativen: `Length` ist die direkteste und effizienteste Methode, die Länge zu ermitteln. Es gibt jedoch auch Möglichkeiten wie eine for-Schleife die Zeichen zu zählen, doch können diese komplizierter und langsamer sein.

(3) Implementierungsdetails: die `Length`-Eigenschaft in C# zählt keine Zeichen, sondern gibt den intern gespeicherten Wert zurück, der beim Erstellen des Strings berechnet und dann gespeichert wird. Das macht die Abfrage besonders schnell.

## Siehe auch

Es gibt auch Funktionen, um die Länge von Arrays und Kollektionen zu ermitteln. Hier sind einige Links dazu:

- String.Length Eigenschaft in der Microsoft Dokumentation: https://docs.microsoft.com/de-de/dotnet/api/system.string.length?view=net-5.0

- Array.Length Eigenschaft in der Microsoft Dokumentation: https://docs.microsoft.com/de-de/dotnet/api/system.array.length?view=net-5.0

- IList< T > .Count Eigenschaft in der Microsoft Dokumentation: https://docs.microsoft.com/de-de/dotnet/api/system.collections.generic.icollection-1.count?view=net-5.0