---
title:                "Suchen und Ersetzen von Text"
html_title:           "C#: Suchen und Ersetzen von Text"
simple_title:         "Suchen und Ersetzen von Text"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Warum

Das Ersetzen von Text in Programmcodes ist ein wichtiger Teil der Softwareentwicklung, da es dabei hilft, Fehler zu korrigieren, Code zu optimieren und insgesamt die Effizienz zu verbessern. Somit ist es ein unverzichtbarer Schritt für jeden Entwickler.

## Wie es geht

Es gibt mehrere Möglichkeiten, Text in C# zu suchen und zu ersetzen. Hier sind drei Beispiele mit entsprechendem Code und Ausgabe:

1. Mit `String.Replace()` können Sie eine bestimmte Stelle im Text durch einen anderen Text ersetzen.

```C#
string text = "Hello World!";
string newText = text.Replace("Hello", "Goodbye");
Console.WriteLine(newText); // Ausgabe: Goodbye World!
```

2. Die Methode `Regex.Replace()` verwendet reguläre Ausdrücke und ist hilfreich, wenn Sie nach einem bestimmten Muster suchen möchten.

```C#
string text = "Today is 25.05.2021";
string newText = Regex.Replace(text, @"\d{2}.\d{2}.\d{4}", "DD.MM.YYYY");
Console.WriteLine(newText); // Ausgabe: Today is DD.MM.YYYY
```

3. Für die Suche und Ersetzung in größeren Texten bietet sich die `StringBuilder`-Klasse an, da sie effizienter ist als die `String`-Klasse.

```C#
StringBuilder sb = new StringBuilder("This is my text");
sb.Replace("my", "your");
Console.WriteLine(sb.ToString()); // Ausgabe: This is your text
```

## Tiefer Einblick

Das Ersetzen von Text mag auf den ersten Blick einfach erscheinen, aber es gibt einige Dinge zu beachten. Beispielsweise kann es bei der Verwendung von regulären Ausdrücken zu Performance-Problemen kommen, daher sollte man diese Methode mit Bedacht einsetzen. Außerdem sollten Sie darauf achten, dass der ersetzte Text nicht versehentlich auch Teil eines anderen Wortes ist, um unerwünschte Effekte zu vermeiden.

## Siehe auch

- Microsoft Dokumentation zu `String.Replace()` (https://docs.microsoft.com/en-us/dotnet/api/system.string.replace)
- Microsoft Dokumentation zu `Regex.Replace()` (https://docs.microsoft.com/en-us/dotnet/api/system.text.regularexpressions.regex.replace)
- Microsoft Dokumentation zu `StringBuilder` (https://docs.microsoft.com/en-us/dotnet/api/system.text.stringbuilder?view=net-5.0)