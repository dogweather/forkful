---
title:                "Verwendung von regulären Ausdrücken"
html_title:           "Haskell: Verwendung von regulären Ausdrücken"
simple_title:         "Verwendung von regulären Ausdrücken"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/haskell/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Warum

Regular Expressions sind ein leistungsstarkes Werkzeug für die Verarbeitung von Text in Programmen. Sie ermöglichen es uns, schnell und effizient Muster in einem Text zu finden, zu extrahieren oder zu ersetzen. Egal ob du ein erfahrener Programmierer bist oder noch am Anfang stehst, Regular Expressions können deine Arbeit erleichtern und dein Code effektiver machen.

## Wie man sie benutzt

Um Regular Expressions in Haskell zu nutzen, müssen wir zunächst das `Text.Regex` Modul importieren:

```Haskell
import Text.Regex
```

Als nächstes können wir ein Regex-Pattern erstellen, indem wir den gewünschten Ausdruck in Strings zwischen `/` eingeben. Anschließend kann der Ausdruck in der Funktion `makeRegex` genutzt werden:

```Haskell
let regex = makeRegex "/[A-Za-z]+/"
```

Jetzt können wir den Regex auf einen Text anwenden, indem wir die Funktion `matchRegex` verwenden:

```Haskell
let text = "Dies ist ein Beispieltext"
matchRegex regex text
```

Die Ausgabe wird eine Liste aller Übereinstimmungen mit dem Ausdruck sein, in diesem Fall "Dies", "ist", "ein" und "Beispieltext".

## Tiefgehende Einblicke

Regular Expressions beinhalten mehr als nur einfache Musterabgleiche. Mit speziellen Zeichen können wir auch Wiederholungen von Mustern definieren, Gruppierungen erstellen und den Ausdruck noch genauer an unsere Bedürfnisse anpassen. Hier sind einige Beispiele:

- `?` bedeutet, dass ein Zeichen vorhergehendes optional ist
- `*` bedeutet, dass das vorhergehende Zeichen beliebig oft wiederholt werden kann
- `+` bedeutet, dass das vorhergehende Zeichen mindestens einmal wiederholt werden muss
- `()` ermöglicht es uns, Gruppen in einem Ausdruck zu erstellen
- `[A-Z]` bedeutet, dass ein beliebiges Zeichen in diesem Bereich akzeptiert wird (hier z.B. alle Großbuchstaben)

Um mehr über die Verwendung von Regular Expressions in Haskell zu erfahren, empfehle ich dir das offizielle Dokumentation oder weitere Tutorials im Internet.

## Siehe auch

- [Offizielle Dokumentation von GHC](https://downloads.haskell.org/~ghc/latest/docs/html/libraries/regex-base-0.93.2/Text-Regex.html)
- [Codebeispiele und Tutorials von Learn You a Haskell](https://learnyouahaskell.com/starting-out#regular-expressions)