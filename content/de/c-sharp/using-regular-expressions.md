---
title:    "C#: Verwenden von regulären Ausdrücken"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Warum

Die Verwendung von regulären Ausdrücken ist eine effektive Möglichkeit, Textmuster innerhalb von Strings abzugleichen und zu manipulieren. Sie sind besonders nützlich für Entwickler, die die Arbeit mit einer großen Menge an Daten oder das Parsen von komplexen Texten vereinfachen möchten.

## So geht's

In diesem Beitrag werden wir lernen, wie man reguläre Ausdrücke in C# verwendet. Um zu beginnen, müssen wir zuerst die "System.Text.RegularExpressions" Namespace importieren. Anschließend können wir mit der Verwendung von regulären Ausdrücken beginnen.

Um einen einfachen Abgleich zu erstellen, können wir die Methode "Matches()" verwenden, die auf einem regulären Ausdruck und einem Eingabestring aufgerufen wird. Diese Methode gibt eine Auflistung von Übereinstimmungen zurück.

```C#
using System;
using System.Text.RegularExpressions;

class Program
{
    static void Main()
    {
        // Zeichenkette erstellen
        string text = "Hallo Welt";

        // regulären Ausdruck definieren
        Regex regex = new Regex("Welt");

        // Abgleich erstellen
        MatchCollection matches = regex.Matches(text);

        // Übereinstimmungen ausgeben
        foreach (Match match in matches)
        {
            Console.WriteLine(match.Value);
        }
    }
}
```

Ausgabe:
```
Welt
```

Reguläre Ausdrücke bieten auch verschiedene Metazeichen und Quantoren, um bessere Abgleichmöglichkeiten zu schaffen. Zum Beispiel verwenden das "+"-Zeichen und das ""-Zeichen das vorherige Element 1 oder beliebig oft. Hier ist ein Beispiel, das alle Ziffern in einem Text erfasst:

```C#
using System;
using System.Text.RegularExpressions;

class Program
{
    static void Main()
    {
        // Zeichenkette erstellen
        string text = "abc123xyz";

        // regulären Ausdruck definieren
        Regex regex = new Regex("[0-9]+");

        // Abgleich erstellen
        MatchCollection matches = regex.Matches(text);

        // Übereinstimmungen ausgeben
        foreach (Match match in matches)
        {
            Console.WriteLine(match.Value);
        }
    }
}
```

Ausgabe:
```
123
```

## Tiefentauchen

Die Verwendung von regulären Ausdrücken erfordert ein Verständnis der verschiedenen Metazeichen und Quantoren, um effektive Abgleiche zu erstellen. Es gibt auch erweiterte Funktionen wie Gruppierungen, Capturing-Gruppen und Rückreferenzen, die es ermöglichen, bestimmte Teile eines Textes zu erfassen und zu manipulieren.

Es ist wichtig zu beachten, dass reguläre Ausdrücke sehr leistungsfähig sind, aber auch sehr komplex werden können. Es ist daher empfehlenswert, sich eingehend mit den verschiedenen Funktionen und Möglichkeiten vertraut zu machen, um sicherzustellen, dass die gewünschten Ergebnisse erzielt werden.

## Siehe auch

- Offizielle Microsoft Dokumentation zu regulären Ausdrücken in C#: https://docs.microsoft.com/en-us/dotnet/standard/base-types/regular-expression-language-quick-reference
- Artikel über die Verwendung von regulären Ausdrücken in C#: https://www.codeproject.com/Articles/9099/The-30-Minute-Regex-Tutorial

Falls Sie Interesse an regulären Ausdrücken haben und ihre Möglichkeiten in C# weiter erkunden möchten, können Sie sich diese weiteren Ressourcen ansehen. Viel Spaß beim Entdecken!