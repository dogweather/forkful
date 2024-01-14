---
title:    "C#: String in Kleinbuchstaben umwandeln"
keywords: ["C#"]
---

{{< edit_this_page >}}

## Warum

Das Konvertieren einer Zeichenfolge in Kleinbuchstaben scheint auf den ersten Blick eine einfache Aufgabe zu sein. Aber warum sollten wir überhaupt eine Zeichenfolge in Kleinbuchstaben umwandeln? Die Antwort ist einfach - Konsistenz und Vergleichbarkeit. Durch die Verwendung von Kleinbuchstaben in einer Zeichenfolge können wir Vergleiche zwischen verschiedenen Zeichenfolgen erleichtern, da Groß- und Kleinschreibung dabei keine Rolle spielen. Dies ist besonders nützlich, wenn wir Benutzereingaben überprüfen oder Datenbankabfragen durchführen.

## Wie geht das?

In C# können wir eine Zeichenfolge mithilfe von `ToLower()` in Kleinbuchstaben konvertieren. Hier ist ein Beispielcode mit einer Beispielausgabe:

```C#
string name = "Johann";
string lowerCaseName = name.ToLower();
Console.WriteLine(lowerCaseName); // gibt "johann" aus
```

In diesem Beispiel haben wir die Zeichenfolge "Johann" in Kleinbuchstaben konvertiert und in der Variable `lowerCaseName` gespeichert. Durch die Verwendung von `ToLower()` können wir sicherstellen, dass die Zeichenfolge unabhängig von der Eingabe des Benutzers immer in Kleinbuchstaben gespeichert wird.

## Tiefergehende Informationen

Bei der Konvertierung einer Zeichenfolge in Kleinbuchstaben gibt es einige Dinge zu beachten. In verschiedenen Sprachen gibt es unterschiedliche Regeln für die Groß- und Kleinschreibung, die bei der Konvertierung berücksichtigt werden müssen. Auch Akzente oder diakritische Zeichen können bei der Konvertierung eine Rolle spielen. Es ist wichtig, dies bei der Verwendung von `ToLower()` zu berücksichtigen, um unerwünschte Fehler zu vermeiden.

Eine weitere Möglichkeit, eine Zeichenfolge in Kleinbuchstaben umzuwandeln, ist die Verwendung der `string.ToLowerInvariant()` Methode. Diese Methode verwendet die invarianter Kultur, was bedeutet, dass sie nicht von aktuellen Spracheinstellungen oder regionalen Einstellungen beeinflusst wird. Dies kann besonders nützlich sein, wenn wir mit verschiedenen Sprachen arbeiten.

## Siehe auch

- [MSDN: ToLower Methode](https://docs.microsoft.com/de-de/dotnet/api/system.string.tolower)
- [MSDN: ToLowerInvariant Methode](https://docs.microsoft.com/de-de/dotnet/api/system.string.tolowerinvariant)