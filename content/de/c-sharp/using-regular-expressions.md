---
title:                "Verwendung von regulären Ausdrücken"
html_title:           "C#: Verwendung von regulären Ausdrücken"
simple_title:         "Verwendung von regulären Ausdrücken"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Warum

Wenn du in deiner C#-Programmierung auf effiziente Weise Textmuster erkennen und manipulieren möchtest, dann können reguläre Ausdrücke (Regular Expressions) dir dabei helfen. Diese leistungsstarken Werkzeuge können dir dabei helfen, komplexe Muster zu identifizieren und damit effektiver zu programmieren.

## Wie geht das

Um reguläre Ausdrücke in C# zu verwenden, musst du zuerst die Klasse `Regex` importieren. Dann kannst du mit Hilfe von Methoden wie `Match()` und `Replace()` spezifische Textmuster suchen und manipulieren. Schau dir das folgende Beispiel an, um zu sehen, wie einfach es ist:

```C#
using System.Text.RegularExpressions;

string text = "Hallo, mein Name ist Max. Ich bin 25 Jahre alt.";

// Finde alle Vokale und ersetze sie durch einen Unterstrich
string transformedText = Regex.Replace(text, "[aeiou]", "_");

// Erwartete Ausgabe: H_ll_, m__n N_m_ _st M_x. _ch b_n 25 J_hr_ _lt.
Console.WriteLine(transformedText);
```

Mit regulären Ausdrücken kannst du auch prüfen, ob ein bestimmtes Muster in deinem Text vorhanden ist oder nicht. Im folgenden Beispiel überprüfen wir, ob die E-Mail-Adresse eines Benutzers in einem bestimmten Format vorliegt:

```C#
using System;
using System.Text.RegularExpressions;

string email = "max.mustermann@example.com";

// Überprüfe, ob die E-Mail-Adresse das richtige Format hat
bool isValidEmail = Regex.IsMatch(email, @"^\w+[\w-\.]*\@\w+((-\w+)|(\w*))\.[a-z]{2,3}$");

if (isValidEmail)
{
    Console.WriteLine("Die E-Mail-Adresse ist gültig.");
}
else
{
    Console.WriteLine("Die E-Mail-Adresse ist ungültig.");
}

// Erwartete Ausgabe: Die E-Mail-Adresse ist gültig.
```

## Tiefere Einsichten

Reguläre Ausdrücke können noch viel mehr als nur einfache Muster zu suchen und zu ersetzen. Du kannst auch mit Hilfe von Gruppen und Capturing-Groups gezielt Teile des Textes extrahieren oder ersetzen. Außerdem gibt es verschiedene spezielle Zeichen, die du in deinen Ausdrücken verwenden kannst, um das Verhalten zu steuern.

Möchtest du mehr über reguläre Ausdrücke erfahren und deine Fähigkeiten in der Verwendung verbessern? Schau dir [diese offizielle C#-Dokumentation](https://docs.microsoft.com/de-de/dotnet/standard/base-types/regular-expressions) an oder finde [hier](https://regex101.com/) praktische Übungen und Beispiele.

## Siehe auch

- [Offizielle C#-Dokumentation zu regulären Ausdrücken](https://docs.microsoft.com/de-de/dotnet/standard/base-types/regular-expressions)
- [Praktische Übungen und Beispiele zu regulären Ausdrücken](https://regex101.com/)