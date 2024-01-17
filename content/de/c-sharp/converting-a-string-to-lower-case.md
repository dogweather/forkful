---
title:                "String in Kleinbuchstaben umwandeln"
html_title:           "C#: String in Kleinbuchstaben umwandeln"
simple_title:         "String in Kleinbuchstaben umwandeln"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Was & Warum?
Beim Konvertieren einer Zeichenfolge in Kleinbuchstaben wird die Großschreibung aller Buchstaben in der Zeichenfolge entfernt. Das kann hilfreich sein, wenn man zum Beispiel die Eingabe des Benutzers standardisieren oder Vergleiche zwischen Zeichenfolgen durchführen möchte.

## Wie geht's?
Um eine Zeichenfolge in Kleinbuchstaben umzuwandeln, verwendet man in C# die `ToLower()`-Funktion. Hier ist ein Beispiel:

```
string str = "Hello World!";
Console.WriteLine(str.ToLower());
```

Die Ausgabe lautet: `hello world!`

## Tief tauchen
Die Umwandlung von Zeichenfolgen in Kleinbuchstaben gibt es schon seit den Anfängen der Programmierung. Heute gibt es jedoch auch andere Methoden, um dies zu erreichen, wie zum Beispiel die Verwendung von regulären Ausdrücken oder die Verwendung von `ToLowerInvariant()`, um die Kultur des Systems nicht zu berücksichtigen.

## Siehe auch
- [Microsoft Dokumentation zu ToLower()](https://docs.microsoft.com/en-us/dotnet/api/system.string.tolower?view=net-5.0)
- [C# Zeichenfolgenmethoden](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/strings/)
- [Reguläre Ausdrücke in C#](https://docs.microsoft.com/en-us/dotnet/standard/base-types/regular-expressions)