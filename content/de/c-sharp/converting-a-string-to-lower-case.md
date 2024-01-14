---
title:    "C#: Umwandlung eines Strings in Kleinbuchstaben"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# Warum

Das Konvertieren von einem String zu Kleinbuchstaben ist eine wichtige Funktion beim Programmieren, da es hilft, Konsistenz in der Formatierung von Texten zu bewahren. Zudem kann es auch bei der Überprüfung von Eingaben oder der Sortierung von Daten nützlich sein.

# Wie geht man vor

```C#
string myString = "DIE Sonne SCHEINT!";
string lowerCase = myString.ToLower();
Console.WriteLine(lowerCase);
```

Das obige Beispiel zeigt, wie einfach es ist, einen String in Kleinbuchstaben umzuwandeln. Der String "myString" wird zuerst erstellt und enthält sowohl Groß- als auch Kleinbuchstaben. Dann verwenden wir die Methode "ToLower()", um den String in Kleinbuchstaben zu konvertieren. Der neue String wird in der Variablen "lowerCase" gespeichert und kann dann ganz einfach ausgegeben werden.

Die Ausgabe des obigen Codes wird sein: "die sonne scheint!". Beachte, dass alle Buchstaben nun klein geschrieben sind.

# Tiefere Einblicke

Beim Konvertieren von einem String zu Kleinbuchstaben gibt es einige wichtige Dinge zu beachten. Zum Beispiel werden bei einigen Sprachen wie Türkisch bestimmte Buchstaben anders konvertiert, da sie ebenfalls Groß- und Kleinbuchstaben haben. Bei der türkischen Sprache ist es wichtig, die Methode "ToLowerCase()" mit dem Sprachparameter zu verwenden, um eine korrekte Konvertierung zu gewährleisten.

Außerdem gibt es verschiedene Methoden, um einen String in Kleinbuchstaben umzuwandeln, wie zum Beispiel "ToLowerInvariant()", "ToLowerInvariant(CultureInfo)" und "ToLowerInvariant(CultureInfo, TextInfo)". Jede dieser Methoden hat ihre eigenen Einsatzmöglichkeiten und sollte entsprechend ausgewählt werden.

# Siehe auch

- [Microsoft Dokumentation über die Methode "ToLower()"](https://docs.microsoft.com/en-us/dotnet/api/system.string.tolower?view=netcore-3.1)
- [Tutorial über das Konvertieren von Strings zu Kleinbuchstaben in C#](https://www.c-sharpcorner.com/article/converting-string-from-uppercase-to-lowercase-case-in-C-Sharp/)
- [Video-Tutorial über die Konvertierung von Strings zu Kleinbuchstaben in C#](https://www.youtube.com/watch?v=9EhUc9ytJhs)