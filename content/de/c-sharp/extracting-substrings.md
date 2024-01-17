---
title:                "Unterstränge extrahieren"
html_title:           "C#: Unterstränge extrahieren"
simple_title:         "Unterstränge extrahieren"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/extracting-substrings.md"
---

{{< edit_this_page >}}

## Was & Warum?
Substring-Extraktion ist im Wesentlichen das Herausholen von Teilzeichenfolgen aus einer größeren Zeichenfolge. Programmierer tun dies, um bestimmte Teile von Zeichenfolgen zu isolieren und für verschiedene Zwecke zu verwenden, z.B. zur Datenverarbeitung oder zur Manipulation von Text.

## Anleitung:
Um Substrings in C# zu extrahieren, können Sie die Methode "Substring" verwenden. Diese Methode akzeptiert zwei Parameter: den Startindex und die Länge des zu extrahierenden Substrings. Zum Beispiel:
```C#
string zeichenfolge = "Hallo Welt!";
string substr = zeichenfolge.Substring(0,5);
Console.WriteLine(substr); // Ausgabe: "Hallo"
```
In diesem Beispiel haben wir den Substring von Index 0 bis einschließlich Index 4 extrahiert. Beachten Sie, dass die Indexierung in C# bei 0 beginnt, daher gibt der Startindex den ersten Buchstaben an und die Länge gibt an, wie viele Zeichen extrahiert werden sollen.

## Tiefer Einblick:
Die Methode "Substring" wurde erstmals in C# 1.0 eingeführt und ist seitdem eine sehr nützliche Funktion für Programmierer. Eine alternative Möglichkeit, Substrings in C# zu extrahieren, ist die Verwendung der "Range"-Syntax, die mit C# 8.0 eingeführt wurde. Diese Syntax ermöglicht es, Teilzeichenfolgen mit einer intuitiveren Notation zu extrahieren, z.B.:
```C#
var zeichenfolge = "Hallo Welt!";
var substr = zeichenfolge[0..5];
Console.WriteLine(substr); // Ausgabe: "Hallo"
```
Hier haben wir dieselbe Extraktion wie im vorherigen Beispiel durchgeführt, aber diesmal mit der neuen "Range"-Syntax. Sie können auch den Endindex statt der Länge angeben, z.B. "0..^6" gibt den Substring von Index 0 bis einschließlich dem 6. letzten Zeichen.

## Siehe auch:
Weitere Informationen zur Methode "Substring" und zur "Range"-Syntax finden Sie in der offiziellen Dokumentation von Microsoft:
https://docs.microsoft.com/dotnet/api/system.string.substring \newline
https://docs.microsoft.com/dotnet/api/system.indexing.range