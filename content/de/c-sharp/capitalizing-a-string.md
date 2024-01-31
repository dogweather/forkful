---
title:                "String in Großbuchstaben umwandeln"
date:                  2024-01-19
html_title:           "C: String in Großbuchstaben umwandeln"
simple_title:         "String in Großbuchstaben umwandeln"

category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Ein String zu kapitalisieren bedeutet, alle Buchstaben in einem Textstring in Großbuchstaben umzuwandeln. Programmierer verwenden diese Methode, um Konsistenz zu gewährleisten oder Text hervorzuheben.

## How to:
C# bietet einfache Möglichkeiten, Strings zu kapitalisieren, wie z.B. die `ToUpper()`-Methode:

```C#
string originalText = "Hallo Welt!";
string capitalizedText = originalText.ToUpper();

Console.WriteLine(capitalizedText);  // Ausgabe: HALLO WELT!
```

Um nur den ersten Buchstaben zu kapitalisieren:

```C#
string originalText = "hallo welt!";
string capitalizedText = char.ToUpper(originalText[0]) + originalText.Substring(1);

Console.WriteLine(capitalizedText);  // Ausgabe: Hallo welt!
```

## Deep Dive:
In früheren Programmiersprachen mussten Entwickler Schleifen und ASCII-Werte benutzen, um Zeichen für Zeichen zu verarbeiten. Heutzutage erleichtern Methoden wie `ToUpper()` und `ToLower()` den Umgang mit Groß- und Kleinschreibung.

Es gibt auch Alternativen zur `ToUpper()`-Methode, wie `ToUpperInvariant()`, die bei der Internationalisierung hilfreich sein kann, da sie die kulturellen Unterschiede in der Zeichenumschreibung ignoriert.

Außerdem ist die Implementierung von `ToUpper()` effizienter als eine manuelle Umsetzung, da sie auf optimierten Systembibliotheken basiert und spezielle Fälle wie Ligaturen und Sonderzeichen berücksichtigt.

## See Also:
- [Microsoft Docs - ToUpper Method](https://docs.microsoft.com/en-us/dotnet/api/system.string.toupper)
- [Microsoft Docs - ToUpperInvariant Method](https://docs.microsoft.com/en-us/dotnet/api/system.string.toupperinvariant)
- [Stack Overflow - Capitalize First Letter of String](https://stackoverflow.com/questions/4135317/make-first-letter-of-a-string-upper-case-for-maximum-performance)
