---
title:                "Einen String großschreiben"
date:                  2024-02-03T19:05:17.875196-07:00
model:                 gpt-4-0125-preview
simple_title:         "Einen String großschreiben"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Was & Warum?
Das Kapitalisieren eines Strings in C# beinhaltet die Umwandlung des ersten Zeichens eines Strings in Großbuchstaben, falls dies nicht bereits der Fall ist. Diese Änderung kann für die Formatierung von Ausgaben, die Durchsetzung von Codierungsstandards oder die Verbesserung der Lesbarkeit von Benutzeroberflächentexten entscheidend sein.

## Wie geht das:
C# bietet einen unkomplizierten Ansatz zur Kapitalisierung von Strings mit eingebauten Methoden. Der einfachste Weg, dies zu erreichen, ist durch direkte Modifikation des Strings mit diesen Methoden. Für komplexere oder spezifischere Kapitalisierungsregeln (z.B. die Kapitalisierung jedes Wortes) könnten zusätzliche Bibliotheken oder manuelle Methoden notwendig sein. Unten sind Beispiele, die zeigen, wie man einen String in C# auf verschiedene Weisen kapitalisiert.

### Grundlegende Kapitalisierung:
Um den ersten Buchstaben eines einzelnen Wortes oder Satzes zu kapitalisieren:

```csharp
string originalString = "hello world";
string capitalizedString = char.ToUpper(originalString[0]) + originalString.Substring(1);
Console.WriteLine(capitalizedString); // Ausgabe: "Hello world"
```

### Kapitalisierung jedes Wortes:
Um den ersten Buchstaben jedes Wortes in einem String zu kapitalisieren, können Sie die Methode `TextInfo.ToTitleCase` verwenden, die im Namespace `System.Globalization` gefunden werden kann:

```csharp
using System;
using System.Globalization;

string originalString = "hello world";
TextInfo textInfo = CultureInfo.CurrentCulture.TextInfo;
string capitalizedString = textInfo.ToTitleCase(originalString);
Console.WriteLine(capitalizedString); // Ausgabe: "Hello World"
```

Hinweis: `ToTitleCase` senkt nicht die Schreibweise der restlichen Buchstaben; es ändert nur den ersten Buchstaben jedes Wortes in Großbuchstaben. Außerdem können bestimmte Wörter nach den Regeln der Titelschreibung (wie "and", "or", "of") je nach Kultureinstellungen nicht kapitalisiert werden.

### Verwendung von Erweiterungsmethoden für Wiederverwendbarkeit:
Sie können eine Erweiterungsmethode für die Klasse `string` erstellen, um den Kapitalisierungsprozess zu vereinfachen, wodurch Ihr Code sauberer und wiederverwendbarer wird. So erstellen und verwenden Sie eine solche Methode:

```csharp
using System;

public static class StringExtensions
{
    public static string Capitalize(this string input)
    {
        if (string.IsNullOrEmpty(input))
        {
            return input;
        }
        return char.ToUpper(input[0]) + input.Substring(1);
    }
}

class Program
{
    static void Main(string[] args)
    {
        string originalString = "hello world";
        string capitalizedString = originalString.Capitalize();
        Console.WriteLine(capitalizedString); // Ausgabe: "Hello world"
    }
}
```

Diese Erweiterungsmethode `Capitalize` kann auf jedes String-Objekt innerhalb des Namensraums aufgerufen werden und bietet einen intuitiveren und objektorientierten Ansatz zur Stringmanipulation in C#.

### Drittanbieter-Bibliotheken:
Während die Standardbibliothek von C# die meisten Bedürfnisse für die Kapitalisierung von Strings abdeckt, könnten bestimmte spezialisierte Aufgaben von Drittanbieter-Bibliotheken profitieren, wie Humanizer. Für die Aufgabe, einfach nur Strings oder jedes Wort in einem String zu kapitalisieren, sind jedoch die Standardmethoden von C# adäquat und effizient, wodurch die Notwendigkeit für externe Abhängigkeiten entfällt.
