---
title:                "Löschen von Zeichen, die einem Muster entsprechen"
html_title:           "C#: Löschen von Zeichen, die einem Muster entsprechen"
simple_title:         "Löschen von Zeichen, die einem Muster entsprechen"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Löschen von Zeichen, die zu einem bestimmten Muster passen, bedeutet, dass Programmierer bestimmte Zeichen aus einem Text entfernen, basierend auf einem vordefinierten Muster. Dies kann nützlich sein, um unerwünschte Zeichen aus Strings oder Dateien zu entfernen oder um bestimmte Muster zu erkennen und zu verarbeiten.

## Wie geht's?

```C#
// Entfernt alle Leerzeichen aus einem String
String text = "Hallo Welt!";
String ohneLeerzeichen = Regex.Replace(text, @"\s+", "");
Console.WriteLine(ohneLeerzeichen); // Output: HalloWelt!
```
    
```C#
// Entfernt alle Ziffern aus einem String
String text = "Ich bin 26 Jahre alt!";
String ohneZiffern = Regex.Replace(text, @"\d+", "");
Console.WriteLine(ohneZiffern); // Output: Ich bin Jahre alt!
```

## Tiefere Einblicke

Das Löschen von Zeichen, die einem bestimmten Muster entsprechen, wird in der Regel mit regulären Ausdrücken (Regular Expressions) realisiert. Diese werden verwendet, um Muster in Texten zu erkennen und zu manipulieren. Es gibt auch andere Möglichkeiten, Zeichen zu löschen, wie z.B. die Verwendung von Schleifen und Bedingungen, aber reguläre Ausdrücke sind in der Regel die effektivste Methode.

## Siehe auch

- [Regex.Replace Methode (C#)](https://docs.microsoft.com/de-de/dotnet/api/system.text.regularexpressions.regex.replace)
- [Reguläre Ausdrücke in 10 Minuten erklärt](https://www.mkyong.com/regular-expressions/java-regex-example/)
- [Reguläre Ausdrücke: Eine kurze Einführung für Programmierer](https://medium.com/@hicraff/regul%C3%A4re-ausdr%C3%BCcke-eine-kurze-einf%C3%BChrung-f%C3%BCr-programmierer-6e731d7779a)