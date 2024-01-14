---
title:                "C#: Verwendung von regulären Ausdrücken"
simple_title:         "Verwendung von regulären Ausdrücken"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Warum

Die Verwendung von regulären Ausdrücken (auch bekannt als "Regex") ist ein leistungsstarkes Werkzeug, das Programmierern hilft, bestimmte Muster in Text zu identifizieren und zu manipulieren. Dies kann besonders nützlich sein, wenn Sie große Mengen von Daten durchsuchen oder bestimmte Daten validieren müssen.

## Wie man es macht

Um reguläre Ausdrücke in C# zu verwenden, müssen Sie zuerst die ```System.Text.RegularExpressions```-Namespace importieren. Dann können Sie die statischen Methoden der ```Regex```-Klasse verwenden, um ein Regex-Objekt zu erstellen und es auf Text anzuwenden. Schauen wir uns ein Beispiel an: 

```C#
using System;
using System.Text.RegularExpressions;

string text = "Hallo, mein Name ist Max und ich bin 25 Jahre alt.";
string regex = "mein Name ist ([A-Z][a-z]+)";

Match match = Regex.Match(text, regex);

if (match.Success)
{
    Console.WriteLine("Der erkannte Name ist: " + match.Groups[1].Value);
}
```

In diesem Beispiel erstellen wir ein Regex-Objekt, das nach einem Namen sucht, der wie ein Vor- und Nachname aufgebaut ist. Dann überprüfen wir, ob es eine Übereinstimmung zwischen dem Text und dem Muster gibt, und geben den erkannten Namen aus.

Die Ausgabe dieses Codes wäre: "Der erkannte Name ist: Max".

## Tiefer Einblick

Die Verwendung von regulären Ausdrücken erfordert ein gutes Verständnis von Mustererkennung und speziellen Zeichen. Es gibt viele verschiedene Zeichen, die verwendet werden können, um Muster zu definieren, und es ist wichtig, diese sorgfältig zu kennen, um die gewünschten Ergebnisse zu erzielen.

Eine Sache, die bei der Verwendung von Regex oft übersehen wird, ist die Möglichkeit, Gruppen zu definieren. Sie können runde Klammern um einen Teil des Musters platzieren, um diesen Teil als separate Gruppe zu identifizieren. Dies ist besonders nützlich, wenn Sie nur an einem bestimmten Teil des Musters interessiert sind und ihn später in Ihrem Code abrufen möchten.

Eine weitere nützliche Funktion von regulären Ausdrücken ist die Möglichkeit, die Ausdrücke bei Bedarf zu modifizieren. Sie können beispielsweise einen Regex mit der Option "IgnoreCase" erstellen, um die Groß- und Kleinschreibung zu ignorieren, oder "Multiline", um mehrzeilige Texte zu verarbeiten.

## Siehe auch
- [Microsoft Docs: Reguläre Ausdrücke in C#](https://docs.microsoft.com/de-de/dotnet/standard/base-types/regular-expressions-in-csharp)
- [C# Programmierhandbuch: Reguläre Ausdrücke](https://docs.microsoft.com/de-de/dotnet/csharp/programming-guide/concepts/regular-expressions)
- [Regex Tester & Debugger](https://regex101.com/)