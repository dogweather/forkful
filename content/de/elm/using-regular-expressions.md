---
title:                "Elm: Die Verwendung von regulären Ausdrücken"
simple_title:         "Die Verwendung von regulären Ausdrücken"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elm/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Warum

Regular Expressions sind ein mächtiges Werkzeug, um Texte zu durchsuchen, zu extrahieren und zu manipulieren. Sie sind besonders nützlich für Programmierer und Webentwickler, die große Textmengen verarbeiten müssen oder komplexe Textmuster finden wollen.

## Wie Man

Um Regular Expressions in Elm zu verwenden, müssen wir das Modul `Regex` importieren. Dann können wir Pattern mit der Funktion `Regex.regex` erstellen und mit dem Text in Verbindung bringen, den wir analysieren möchten. Hier ist ein Beispiel, das überprüft, ob die E-Mail-Adresse in einem String gültig ist:

```
Elm importieren
Regex
```
```
input = "meine-email@beispiel.com"
myRegex = Regex.regex "[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}"
Regex.betrachten (Regex.find myRegex input)

-- Ausgabe: Just "meine-email@beispiel.com"
```

## Tiefer Tauchen

Es gibt viele verschiedene Möglichkeiten, Regular Expressions in Elm zu verwenden. Hier sind einige wichtige Konzepte, die es zu beachten gilt:

- `Regex` -Modul bietet Funktionen wie `Regex.map`, `Regex.replace` und` Regex.split`, um mit Übereinstimmungen umzugehen, wenn sie gefunden werden.
- Wir können eingebaute Zeichengruppen wie `\\d` (Zahl),` \\w` (Alphanumerisch) und` \\s` (Leerzeichen) verwenden, um die Muster abzukürzen.
- Quantifikatoren wie `*` (beliebig oft wiederholen), `+` (mindestens einmal wiederholen) und `{n}` (genau n Mal wiederholen) ermöglichen es uns, die Anzahl der übereinstimmenden Zeichen flexibel zu definieren.

Für eine vollständige Liste der Funktionen und Syntax von Regular Expressions in Elm empfehle ich, die offizielle [Dokumentation](https://package.elm-lang.org/packages/elm/core/latest/Regex) zu lesen.

## Siehe Auch

- [Die wichtige Rolle von Regular Expressions](https://medium.com/@stonemakespace/the-important-role-of-regular-expressions-b72c9a5baaa6) von Stone MakeSpace
- [Verwendung von Regular Expressions in Elm](https://dev.to/timLogs/using-regular-expressions-in-elm-2pp1) von Tim Theriault