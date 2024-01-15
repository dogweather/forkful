---
title:                "Umwandeln eines Strings in Kleinbuchstaben"
html_title:           "C#: Umwandeln eines Strings in Kleinbuchstaben"
simple_title:         "Umwandeln eines Strings in Kleinbuchstaben"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Warum

Ganz einfach: Manchmal müssen wir einfach Strings in Kleinbuchstaben umwandeln, sei es für die Dateneingabe, Formatierung oder für Vergleiche. Glücklicherweise bietet C# einige praktische Methoden für diese Aufgabe.

## So geht's

Um einen String in Kleinbuchstaben umzuwandeln, können wir die `ToLower()` Methode verwenden. Sie wird auf einem String aufgerufen und gibt eine neue Zeichenfolge mit allen Buchstaben in Kleinbuchstaben zurück.

```C#
string name = "MAX MUSTERMANN";
string lowerCaseName = name.ToLower();

Console.WriteLine(lowerCaseName);

// Output: max mustermann
```

Um die Ausgabe des Strings in der Konsole oder in einem anderen Programm anders zu formatieren, können wir die `ToLower()` Methode innerhalb der `Console.WriteLine()` Methode verwenden.

```C#
string name = "MAX MUSTERMANN";

Console.WriteLine("Hallo {0}!", name.ToLower());

// Output: Hallo max mustermann!
```

## Tiefer eintauchen

Bei der Verwendung der `ToLower()` Methode müssen wir beachten, dass sie nicht nur alle Buchstaben in Kleinbuchstaben umwandelt, sondern auch alle Sonderzeichen und Leerzeichen unverändert lässt. Das bedeutet, dass zum Beispiel ein String mit dem Namen "Max Mustermann" immer noch den Punkt und das Leerzeichen zwischen Vor- und Nachnamen behalten wird.

Wenn wir sicherstellen wollen, dass der komplette String in Kleinbuchstaben ist, müssen wir vorher noch die `Trim()` Methode anwenden, um mögliche Leerzeichen am Anfang oder Ende des Strings zu entfernen.

```C#
string name = "  MAX MUSTERMANN  ";
string lowerCaseName = name.Trim().ToLower();

Console.WriteLine(lowerCaseName);

// Output: max mustermann
```

## Siehe auch

- [C# String Dokumentation](https://docs.microsoft.com/en-us/dotnet/api/system.string?view=netcore-3.1)
- [C# String Manipulation Tutorial](https://www.tutorialspoint.com/csharp/csharp_strings.htm)