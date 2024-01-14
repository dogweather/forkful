---
title:                "Swift: Zufallszahlen erzeugen"
programming_language: "Swift"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/swift/generating-random-numbers.md"
---

{{< edit_this_page >}}

#
Warum: Warum Sie sich mit der Generierung von Zufallszahlen beschäftigen sollten

Das Generieren von Zufallszahlen kann in der Programmierung eine wichtige Rolle spielen, zum Beispiel bei der Gestaltung von Spielen oder bei der Erstellung von Testdaten. Es kann auch hilfreich sein, um bestimmte Algorithmen wie den Monte-Carlo-Algorithmus zu implementieren. In diesem Blogbeitrag werden wir uns ansehen, wie man mit Swift randomisierte Zahlen erstellt.

## Wie man in Swift Zufallszahlen generiert

Um in Swift Zufallszahlen zu generieren, gibt es mehrere Möglichkeiten. Eine einfache Möglichkeit ist die Verwendung der `arc4random_uniform()`-Funktion, die eine zufällige Ganzzahl im Bereich von 0 bis zu einem angegebenen Maximum zurückgibt. Hier ist ein Beispiel, wie man diese Funktion in Swift verwendet:

```Swift
let randomNum = arc4random_uniform(10)
print(randomNum) // Beispiel-Ausgabe: 7
```

Eine weitere Option ist die Verwendung der `random()`-Funktion, die eine zufällige Gleitkommazahl zwischen 0 und 1 zurückgibt. Hier ist ein Beispiel, wie man diese Funktion verwendet:

```Swift
let randomNum = Double.random(in: 0...1)
print(randomNum) // Beispiel-Ausgabe: 0.758979373
```

Andere hilfreiche Funktionen sind `arc4random()` für die Generierung von zufälligen Ganzzahlen und `randomElement()` für die Auswahl eines zufälligen Elements aus einer Sammlung.

Es gibt auch Möglichkeiten, die Randomisierung in Swift noch weiter zu verfeinern, z.B. durch die Verwendung von Seeds, die die generierten Zufallszahlen beeinflussen.

## Tiefere Einblicke in die Generierung von Zufallszahlen

Wenn Sie tiefer in die Generierung von Zufallszahlen in Swift eintauchen möchten, gibt es einige interessante Aspekte zu beachten. Zum Beispiel ist es wichtig zu wissen, dass in Swift standardmäßig die Mersenne-Twister-Algorithmus-Implementierung zur Erzeugung von Zufallszahlen verwendet wird. Dies garantiert eine gleichmäßigere Verteilung der Zahlen im Vergleich zu anderen Algorithmen.

Es gibt auch verschiedene Optionen für die Genauigkeit der generierten Zufallszahlen. Zum Beispiel können Sie mit `random()` nicht nur Zahlen im Bereich von 0 bis 1, sondern auch im Bereich von -1 bis 1 mit Gleitkommagenauigkeit generieren.

Eine andere wichtige Überlegung ist die Verwendung von Zufallszahlen in sicherheitsrelevanten Anwendungen. In solchen Fällen sollten Sie prüfen, ob die verwendeten Zufallszahlengeneratoren kryptografisch sicher sind.

## Siehe auch

[Die offizielle Dokumentation zu randomisierten Zahlen in Swift](https://docs.swift.org/swift-book/LanguageGuide/TheBasics.html#ID340)

[Ein Tutorial zur Verwendung von Zufallszahlen in Swift](https://www.swiftbysundell.com/tips/random-numbers/)

[Weitere Infos über den Mersenne-Twister-Algorithmus](https://en.wikipedia.org/wiki/Mersenne_Twister)