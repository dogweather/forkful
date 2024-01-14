---
title:    "C#: Verwendung von regulären Ausdrücken"
keywords: ["C#"]
---

{{< edit_this_page >}}

## Warum
Wenn Sie regelmäßig mit Texten in Ihrer C#-Programmierung arbeiten, können reguläre Ausdrücke eine äußerst nützliche Funktion sein. Sie ermöglichen es Ihnen, Muster in Strings zu suchen und zu manipulieren, was Ihnen viel Zeit und Aufwand ersparen kann. In diesem Blog-Beitrag werfen wir einen Blick auf die Verwendung von regulären Ausdrücken in C# und wie sie Ihr Codierungsleben erleichtern können.

## Wie geht man vor?
Um reguläre Ausdrücke in C# zu verwenden, müssen Sie zunächst die System.Text.RegularExpressions Namespace importieren. Dann können Sie den regulären Ausdruck in einem regulären Ausdrucksmuster (Regex) Objekt speichern und die verschiedenen Funktionen dieses Objekts verwenden, um Textmuster zu finden oder zu ersetzen.

```C#
using System.Text.RegularExpressions;

// regulärer Ausdruck definieren
Regex regex = new Regex(@"\d{4}-\d{2}-\d{2}");

// Eingabetext durchsuchen
Match match = regex.Match("Heute ist der 01-02-2021.");

// Ausgabe der gefundenen Übereinstimmung
Console.WriteLine(match.Value); // Output: "01-02-2021"
```

Wie Sie sehen, können Sie mit regulären Ausdrücken spezifische Muster in einem String finden. Sie können auch durch Verwendung von Metazeichen wie dem Stern (*) und dem Plus (+) mehrere Wiederholungen eines bestimmten Musters finden. Zum Beispiel würde der reguläre Ausdruck ```[a-z]+``` alle Wörter in einem String finden.

## Tiefergehende Einblicke
Reguläre Ausdrücke können auch verwendet werden, um Strings zu manipulieren und anzupassen. Zum Beispiel können Sie mit der Funktion ```Regex.Replace()``` bestimmte Teile oder Muster in einem String ersetzen.

```C#
// Eingabetext formatieren
string input = "Bitte geben Sie Ihre Telefonnummer ein: 123-456-7890.";
Regex regex = new Regex(@"(\d{3})-(\d{3})-(\d{4})");

// neue Telefonnummer erstellen
string newNumber = regex.Replace(input, "($1) $2-$3");

// Ausgabe des neuen Strings
Console.WriteLine(newNumber); // Output: "Bitte geben Sie Ihre Telefonnummer ein: (123) 456-7890."
```

Dies ist nur ein Beispiel dafür, wie Sie reguläre Ausdrücke verwenden können, um Strings zu formatieren und anzupassen. Es gibt viele weitere Funktionen und Möglichkeiten, die Sie entdecken können, um Ihren Code effizienter zu gestalten.

## Siehe auch
- [Microsoft Dokumentation zu regulären Ausdrücken in C#](https://docs.microsoft.com/en-us/dotnet/standard/base-types/regular-expression-language-quick-reference)
- [C# Tutorials auf Tutorialspoint](https://www.tutorialspoint.com/csharp)
- [C# Kurs auf Codecademy](https://www.codecademy.com/learn/learn-c-sharp)