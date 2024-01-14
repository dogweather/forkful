---
title:                "C#: Umwandlung eines Strings in Kleinbuchstaben"
simple_title:         "Umwandlung eines Strings in Kleinbuchstaben"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Warum

Wenn Sie jemals mit Zeichenketten in C# gearbeitet haben, haben Sie vielleicht bemerkt, dass es manchmal notwendig ist, alle Buchstaben in Kleinbuchstaben zu ändern. Dies kann hilfreich sein, um Vergleiche zwischen Zeichenketten durchzuführen oder einfach um ein konsistentes Format für Ihre Daten zu haben. In diesem Blogbeitrag werden wir uns ansehen, wie Sie eine Zeichenkette in C# in Kleinbuchstaben umwandeln können.

## Wie man eine Zeichenkette in Kleinbuchstaben umwandelt

Um eine Zeichenkette in Kleinbuchstaben umzuwandeln, können wir die eingebaute Methode `ToLower()` verwenden. Diese Methode gibt eine neue Zeichenkette zurück, in der alle Buchstaben in Kleinbuchstaben umgewandelt wurden. Hier ist ein Beispiel, wie Sie dies in Ihrem Code verwenden können:

```C#
string myString = "Hallo, WELT!";
string lowercaseString = myString.ToLower();

Console.WriteLine(lowercaseString);

// Output: hallo, welt!
```

Wie Sie sehen, gibt die Methode `ToLower()` eine neue Zeichenkette zurück, ohne die ursprüngliche zu ändern. Dies ist wichtig zu beachten, da Zeichenketten in C# unveränderlich sind, was bedeutet, dass sie nicht geändert werden können, sobald sie erstellt wurden.

## Eine tiefere Eintauchen in die Zeichenketten-Konvertierung

Die Methode `ToLower()` ist sehr nützlich, wenn Sie eine einfache Konvertierung von Großbuchstaben in Kleinbuchstaben benötigen. Es gibt jedoch einige Fälle, in denen Sie möglicherweise eine andere Art der Konvertierung benötigen, z.B. wenn Sie mit Sprachen arbeiten, die Sonderzeichen oder Umlaute enthalten. In solchen Fällen kann die Verwendung von `ToLower()` möglicherweise nicht die gewünschten Ergebnisse liefern.

Eine alternative Methode ist die Verwendung von `ToLowerInvariant()`. Diese Methode verwendet immer dieselbe Kultur, unabhängig von der aktuellen Systemkultur, um eine konvertierte Zeichenkette zurückzugeben. Dies kann nützlich sein, wenn Sie sicherstellen möchten, dass Ihre Zeichenketten in einer bestimmten Kultur konsistent konvertiert werden.

Beispiel:

```C#
string myString = "Möglichkeit";
string lowercaseString = myString.ToLowerInvariant();

Console.WriteLine(lowercaseString);

// Ausgabe: möglichkeit
```

Wie Sie sehen, wird hier die deutsche Umlaut "Ö" ordnungsgemäß in "ö" konvertiert. Dies kann jedoch je nach Kontext unterschiedlich sein, daher ist es wichtig, die beste Methode für Ihre spezifischen Anforderungen auszuwählen.

## Siehe auch

- [Offizielle Dokumentation zur Methode ToLower() in C#](https://docs.microsoft.com/de-de/dotnet/api/system.string.tolower?view=net-5.0)
- [Offizielle Dokumentation zur Methode ToLowerInvariant() in C#](https://docs.microsoft.com/de-de/dotnet/api/system.string.tolowerinvariant?view=net-5.0)
- [Artikel über den Umgang mit Zeichenketten in C#](https://www.codecademy.com/articles/strings-csharp)