---
title:                "Eine Zeichenkette interpolieren."
html_title:           "C#: Eine Zeichenkette interpolieren."
simple_title:         "Eine Zeichenkette interpolieren."
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/interpolating-a-string.md"
---

{{< edit_this_page >}}

# Was und warum?

Eine Zeichenfolgeninterpolation ermöglicht es uns, dynamische Werte in eine Zeichenfolge einzufügen. Wir können Variablen, Ausdrücke oder sogar Methodenaufrufe direkt in unsere Zeichenfolge einbetten. Dadurch wird der Prozess der Zeichenfolgenformatierung in C# vereinfacht und wir können die Lesbarkeit unseres Codes verbessern.

## Wie geht's?

Um eine Zeichenfolge zu interpolieren, müssen wir diese mit dem `$`-Zeichen kennzeichnen und die dynamischen Werte in geschweifte Klammern `{} `einschließen, wie im folgenden Beispiel:

```C#
int age = 25;
string message = $"Ich bin {age} Jahre alt.";
Console.WriteLine(message);

```
Dieser Code wird die Ausgabe `"Ich bin 25 Jahre alt."`erzeugen.

Wir können auch Ausdrücke verwenden, um zusammengesetzte Werte zu interpolieren, wie z.B.:

```C#
int radius = 5;
double area = Math.PI * radius * radius;
string message = $"Der Flächeninhalt eines Kreises mit dem Radius {radius} beträgt {area:F2} Quadratmeter.";
Console.WriteLine(message);

```
Die Ausgabe wird in diesem Fall `"Der Flächeninhalt eines Kreises mit dem Radius 5 beträgt 78,54 Quadratmeter."` sein.

## Tief tauchen

Die Zeichenfolgeninterpolation wurde in C# 6 eingeführt und war eine Verbesserung gegenüber der früheren Zeichenfolgenformatierung mit dem `String.Format()`-Methode. Es gibt auch andere Möglichkeiten, dynamische Werte in Zeichenfolgen einzufügen, wie z.B. die Konkatenation von Zeichenfolgen mit dem `+`-Operator oder die Verwendung von `String.Format()`.

Die Interpolation ist jedoch die bevorzugte Methode, da sie kürzer und lesbarer ist. Sie bietet auch zusätzliche Funktionen wie die Formatierung von numerischen Werten und die Verwendung von benannten Argumenten.

Die Interpolation wird auch als syntaktischer Zucker bezeichnet, da sie letztendlich in die `String.Format()`-Methode übersetzt wird. Sie ist jedoch einfacher zu verwenden und zu lesen.

## Siehe auch

- [Offizielle Dokumentation zur Zeichenfolgeninterpolation in C#](https://docs.microsoft.com/de-de/dotnet/csharp/tutorials/string-interpolation)
- [Einführung in die Zeichenfolgeninterpolation in C#](https://www.tutorialsteacher.com/csharp/csharp-string-interpolation)
- [Unterschied zwischen Zeichenfolgeninterpolation und Zeichenfolgenformatierung mit String.Format()](https://stackoverflow.com/questions/15505462/what-is-the-difference-between-string-format-and-string-interpolation-in-c)