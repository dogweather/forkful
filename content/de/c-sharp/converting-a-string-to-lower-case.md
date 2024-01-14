---
title:                "C#: Eine Zeichenfolge in Kleinbuchstaben umwandeln."
programming_language: "C#"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Warum

Das Konvertieren eines Strings in Kleinbuchstaben ist ein grundlegendes Konzept in der Programmierung, das hilfreich sein kann, um Konsistenz und Genauigkeit in Ihren Code zu gewährleisten. Durch die Konvertierung von Strings in Kleinbuchstaben können Sie sicherstellen, dass Vergleiche und Suchvorgänge nicht durch unterschiedliche Groß- und Kleinschreibung beeinflusst werden.

## Wie geht das?

Um einen String in Kleinbuchstaben umzuwandeln, können Sie die integrierte Methode "ToLower()" verwenden. Diese Methode wandelt alle Buchstaben in einem String in Kleinbuchstaben um.

```C#
string name = "Max Mustermann";
string lowerCaseName = name.ToLower();

Console.WriteLine(lowerCaseName); // Ausgabe: max mustermann
```

## Tiefer Einblick

Es gibt mehrere Fälle, in denen die Konvertierung von Strings in Kleinbuchstaben nützlich sein kann. Zum Beispiel können Sie die Methode "ToLower()" auch verwenden, um Eingaben von Benutzern in einem einheitlichen Format zu erfassen, unabhängig von der Art und Weise, wie sie den Text eingegeben haben.

Darüber hinaus kann die Verwendung von Kleinbuchstaben auch bei der Validierung von Benutzereingaben hilfreich sein, da es einfacher ist, auf mögliche Tippfehler zu prüfen.

Es ist jedoch wichtig zu beachten, dass bei der Verwendung der "ToLower()" Methode die kulturellen Einstellungen berücksichtigt werden sollten, da in einigen Sprachen Buchstaben anders in Kleinbuchstaben geschrieben werden können.

## Siehe auch

- [String.ToLower() Methode Dokumentation](https://docs.microsoft.com/de-de/dotnet/api/system.string.tolower?view=net-5.0)
- [Microsoft C# Programmierleitfaden](https://docs.microsoft.com/de-de/dotnet/csharp/)