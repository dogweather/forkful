---
title:                "Teilzeichenketten extrahieren"
html_title:           "PowerShell: Teilzeichenketten extrahieren"
simple_title:         "Teilzeichenketten extrahieren"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/extracting-substrings.md"
---

{{< edit_this_page >}}

# Extrahieren von Teilzeichenketten in C#

## Was & Warum?
Extrahieren von Teilzeichenketten bedeutet, bestimmte Abschnitte einer Zeichenkette in einer neuen Zeichenkette zu isolieren. Es ist nützlich, um bestimmte Informationsblöcke zu filtern oder zu bearbeiten, ohne die gesamte Zeichenkette zu manipulieren.

## Wie man:
Nutzen Sie die `Substring` Methode in C#. Hier ist ein simples Beispiel:

```C#
string str = "Hallo, Welt!";
string substr = str.Substring(7, 5);
Console.WriteLine(substr);
```
Ausgabe des Programms wird "`Welt!`" sein.

## Tiefgang
Zum geschichtlichen Kontext, die `Substring` Methode existiert schon seit der Anfangszeit von C# und .NET, was ihre Beliebt- und Vertrautheit erklärt. Als Alternativen könnten `Split`, `IndexOf` oder reguläre Ausdrücke verwendet werden, aber diese haben tendenziell mehr Aufwand und Komplexität.

Beim Extrahieren einer Teilzeichenkette wird eigentlich eine ganz neue Zeichenkette erstellt. Das liegt daran, dass Zeichenketten in C# unveränderlich sind, was bedeutet, dass sie nach ihrer Erstellung nicht geändert werden können. 

## Siehe auch
Weitere Informationen und detailliertere Anleitungen finden Sie unter den folgenden Links:

1. Microsoft Dokumentation: [Substring Methode](https://docs.microsoft.com/de-de/dotnet/api/system.string.substring?view=net-5.0)
2. StackOverflow Diskussion: [Substring vs Split Methode](https://stackoverflow.com/questions/298830/split-string-containing-command-line-parameters-into-string-in-c-sharp)
3. C# Station Tutorial: [Arbeiten mit Zeichenketten](https://csharp-station.com/Tutorial/CSharp/Lesson13)