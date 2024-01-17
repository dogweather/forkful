---
title:                "Einen String großschreiben"
html_title:           "C#: Einen String großschreiben"
simple_title:         "Einen String großschreiben"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Kapitalisieren einer Zeichenkette ist ein üblicher Schritt in der Programmierung, bei dem der erste Buchstabe eines jeden Wortes großgeschrieben wird. Dies wird gemacht, um die Lesbarkeit und Ästhetik der Zeichenkette zu verbessern und es einfacher zu machen, Schlüsselwörter oder Variablennamen zu identifizieren. 

## Wie geht's?
Die Verwendung von C# macht das Kapitalisieren einer Zeichenkette sehr einfach. Hier ist ein Beispielcode:

```C#
// Eingabezeichenkette
string eingabe = "hallo welt";
// Ausgabezeichenkette
string ausgabe = eingabe.ToUpper();
Console.WriteLine(ausgabe);
```

Dieser Code wird die Zeichenkette "hallo welt" in "HALLO WELT" umwandeln.

## Tiefer Einblick
Das Prinzip der String-Kapitalisierung wurde erstmals in den Programmiersprachen COBOL und BASIC eingeführt, um die Dateinamen im Dateisystem zu formatieren. Obwohl es nicht zwingend notwendig ist, wird das Kapitalisieren einer Zeichenkette in der Programmierung allgemein als bewährte Methode angesehen, um die Lesbarkeit und Konsistenz des Codes zu verbessern.

Alternativ können auch andere Methoden wie z.B. die Verwendung von Unterstrichen oder CamelCase verwendet werden, um Wörter in Variablennamen zu trennen. Es ist jedoch wichtig, dass innerhalb eines bestimmten Projekts eine einheitliche Methode verwendet wird, um die Lesbarkeit und Verständlichkeit des Codes zu gewährleisten.

In C# wird die String-Kapitalisierung durch einen eingebauten Befehl ("ToUpper") ermöglicht, der jede Zeichenkette in Großbuchstaben umwandelt. Dieser Befehl kann jedoch nicht angewendet werden, wenn Sonderzeichen oder Zahlen in der Zeichenkette vorhanden sind.

## Siehe auch
- [MSDN-Dokumentation zu String.ToUpper()](https://docs.microsoft.com/en-us/dotnet/api/system.string.toupper?view=net-5.0)
- [Alternative Methoden zur Formatierung von Zeichenketten](https://stackoverflow.com/questions/3035338/c-sharp-how-to-set-title-case)
- [Begriffe und Konventionen in der Programmierung](https://en.wikipedia.org/wiki/Naming_convention_(programming))