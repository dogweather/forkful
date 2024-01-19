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

# Suchen und Ersetzen von Text in C#: Ein kompakter Leitfaden

## Was & Warum?

Die Suche und Ersetzung von Text ist ein grundlegender Vorgang, der den Text in Datenströmen oder Textdateien durchsucht und Teile durch andere Teile ersetzt. Programmierer machen das, um Daten zu manipulieren, Fehler zu korrigieren oder Textformatierungen zu ändern.

## So geht's:

Die Methode `Replace` einer Zeichenkette kann zum Suchen und Ersetzen von Text verwendet werden. Hier ist ein einfacher Codeausschnitt, der zeigt, wie das geht:

```C#
string text = "Hallo Welt!";
string neuerText = text.Replace("Welt", "C#");
Console.WriteLine(neuerText);  // Ausgabe: Hallo C#!
```
In diesem Beispiel wird "Welt" im ursprünglichen Text durch "C#" ersetzt.

## Vertiefung

Historisch gesehen gibt es die Suche und Ersetzung von Text seit der Einführung von Textbearbeitungssoftware. Es handelt sich um eine essentielle Funktion, die von fortgeschrittenen Algorithmen bis hin zu einfachen Texteditoren reicht.

Es gibt auch alternative Methoden für komplexere Situationen. Zum Beispiel, wenn Sie reguläre Ausdrücke (Regex) verwenden möchten, können Sie die `Regex.Replace`-Methode verwenden:

```C#
using System.Text.RegularExpressions;
...
string text = "10 Äpfel";
string neuerText = Regex.Replace(text, @"\d+", "viele");
Console.WriteLine(neuerText);  // Ausgabe: viele Äpfel
```
In diesem Beispiel ersetzt der Code alle Zahlen im Text durch das Wort "viele".

Zur Implementierung: Die `string.Replace`-Methode in C# verwendet den schnellen Stringsuchalgorithmus von Boyer-Moore unter der Haube.

## Siehe auch

Um mehr über die Verwendung von `string.Replace` und `Regex.Replace` in C# zu erfahren, schauen Sie sich die entsprechenden Seiten in der MSDN-Dokumentation an:

1. [String.Replace Methode](https://docs.microsoft.com/de-de/dotnet/api/system.string.replace?view=net-6.0)
2. [Regex.Replace Methode](https://docs.microsoft.com/de-de/dotnet/api/system.text.regularexpressions.regex.replace?view=net-6.0) 

Für tiefere Einblicke in string Manipulationen, lesen Sie das Kapitel "String Manipulation und Suchalgorithmen" im Buch "C# in Depth" von Jon Skeet.