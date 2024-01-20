---
title:                "Reguläre Ausdrücke verwenden"
html_title:           "Bash: Reguläre Ausdrücke verwenden"
simple_title:         "Reguläre Ausdrücke verwenden"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/using-regular-expressions.md"
---

{{< edit_this_page >}}

--------

# Wie nutzt man reguläre Ausdrücke in C#?

--------

## Was & Warum?

Reguläre Ausdrücke (auch bekannt als Regex) sind Muster, die zur Übereinstimmung oder Suche nach Strings verwendet werden. Sie sind ein kraftvolles Werkzeug für Entwickler, um Textmanipulation und Validierungsaufgaben effektiv zu handhaben.

## Wie geht es?

Hier ist ein Beispiel, wie man einen Regulären Ausdruck in C# verwendet:

```C#
using System;
using System.Text.RegularExpressions;

class Program
{
    static void Main()
    {
        // Erstellen einer Instanz von Regex
        Regex regex = new Regex(@"\b[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Za-z]{2,}\b");

        // Prüfen von E-Mail-Adressen
        Console.WriteLine(regex.IsMatch("info@beispiel.de")); // Gibt "True" zurück
        Console.WriteLine(regex.IsMatch("beispiel.de")); // Gibt "False" zurück
    }
}
```

In diesem Beispiel verwendet wird ein Muster erstellt, das E-Mail-Adressen erkennt. Wenn eine gültige E-Mail-Adresse eingegeben wird, gibt die Methode `IsMatch` `True` zurück.

## Tiefgehend

Der Ursprung der Regulären Ausdrücke liegt in der theoretischen Informatik und sie haben sich seitdem zu einem Standardwerkzeug in der Softwareentwicklung entwickelt. 

Eine Alternative zu Regex ist die manuelle Stringmanipulation und Suche, die jedoch oft ineffizient und fehleranfällig ist. 

Die Implementierung von Regulären Ausdrücken in C# erfolgt durch die `System.Text.RegularExpressions` Klassenbibliothek. Die wichtigsten Methoden in dieser Bibliothek sind `Match`, `Matches`, `IsMatch`, `Replace` und `Split`.

## Siehe Auch

Für weitere Informationen und detaillierte Erläuterungen empfehle ich folgende Ressourcen:

1. [Microsoft Dokumentation zu regulären Ausdrücken in C#](https://docs.microsoft.com/de-de/dotnet/standard/base-types/regular-expressions)
2. [RegExr: Learn, Build, & Test RegEx](https://regexr.com/)
3. [Stack Overflow: Regular Expression Tag](https://stackoverflow.com/questions/tagged/regex)

--------

**Ende des Artikels**