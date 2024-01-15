---
title:                "Unterstrings extrahieren"
html_title:           "C#: Unterstrings extrahieren"
simple_title:         "Unterstrings extrahieren"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/extracting-substrings.md"
---

{{< edit_this_page >}}

## Warum

Warum sollte man sich damit beschäftigen, Substrings zu extrahieren? Nun, das Extrahieren von Substrings ist eine nützliche Funktion, um bestimmte Teile von Zeichenketten zu isolieren. Dies kann hilfreich sein, um Daten zu verarbeiten oder bestimmte Muster in Texten zu finden.

## Wie geht man vor?

Um Substrings in C# zu extrahieren, kannst du die "Substring" Methode verwenden. Diese erlaubt es dir, einen Teil einer Zeichenkette basierend auf ihrer Start- und Endposition zu extrahieren.

```
// Beispiel einer Zeichenkette
string text = "Ich liebe Programmieren!"

// Extrahiere "liebe"
string substring = text.Substring(3, 5);

// Gib den extrahierten Substring aus
Console.WriteLine(substring); // Ausgabe: "liebe"
```

Um bestimmte Muster zu finden, kannst du auch die "IndexOf" Methode zusammen mit der "Substring" Methode verwenden. Diese sucht nach einem bestimmten Zeichen oder einer Zeichenfolge in einer Zeichenkette und gibt die Position des ersten Vorkommens zurück.

```
// Beispiel einer Zeichenkette
string text = "C#'s Substring Methode macht Spaß!";

// Finde die Position von "Substring"
int position = text.IndexOf("Substring");

// Extrahiere den Text ab der gefundenen Position
string substring = text.Substring(position);

// Gib den extraherten Substring aus
Console.WriteLine(substring); // Ausgabe: "Substring Methode macht Spaß!"
```

## Tiefer Einblick

Die "Substring" Methode ist nicht nur auf statische Werte beschränkt, sondern kann auch dynamisch verwendet werden. Du kannst zum Beispiel die Länge des Strings berechnen und diese als Endposition für die "Substring" Methode verwenden, um immer nur den letzten Teil der Zeichenkette zu extrahieren.

```
// Beispiel einer Zeichenkette
string text = "Hallo, ich bin ein Text!";

// Bestimme die Länge des Strings
int length = text.Length; // Ausgabe: 23

// Extrahiere den letzten Teil der Zeichenkette
string last = text.Substring(length - 5, 5); // Ausgabe: "Text!"

// Gib den extrahierten Substring aus
Console.WriteLine(last); // Ausgabe: "Text!"
```

Es gibt auch zusätzliche Optionen innerhalb der "Substring" Methode, wie zum Beispiel die Angabe einer Überlappung, wenn du mehrere Teilstrings aus einer Zeichenkette extrahieren möchtest.

## Siehe auch

- MSDN Dokumentation zur "Substring" Methode: https://docs.microsoft.com/de-de/dotnet/api/system.string.substring
- Artikel zum Thema "Zeichenketten in C#": https://www.lernmoment.de/csharp-programmieren/zeichenketten-in-csharp/