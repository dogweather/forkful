---
title:                "Swift: Zufallszahlen erzeugen"
simple_title:         "Zufallszahlen erzeugen"
programming_language: "Swift"
category:             "Swift"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/swift/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Warum

In der Welt der Programmierung gibt es immer wieder Aufgaben, die zufällige Zahlen erfordern, sei es für die Simulation von Spielen oder für die Erzeugung von Passwörtern. In diesem Blogbeitrag werden wir uns daher damit beschäftigen, wie man in Swift Zufallszahlen generieren kann.

# Wie geht man vor?

Um in Swift Zufallszahlen zu generieren, gibt es verschiedene Möglichkeiten. Eine einfache Methode ist die Verwendung der `random()` Funktion. Diese gibt eine zufällige Zahl zwischen 0 und 1 zurück. Hier ein Beispiel:

```Swift
let randomNumber = random()
print(randomNumber) // Output: 0.713523
```

Wenn man eine zufällige ganze Zahl braucht, kann man die `Int` Methode verwenden. Diese nimmt als Parameter einen Bereich an, aus dem die Zufallszahl gezogen werden kann. Im folgenden Beispiel wird eine zufällige Zahl zwischen 1 und 10 generiert:

```Swift
let randomInt = Int.random(in: 1...10)
print(randomInt) // Output: 7
```

Wenn man eine zufällige Zahl mit Nachkommastellen braucht, kann man die `Double` Methode nutzen. Diese funktioniert ähnlich wie die `Int` Methode, nimmt jedoch einen `Double` als Parameter an. Im folgenden Beispiel wird eine zufällige Zahl zwischen 1 und 100 mit zwei Nachkommastellen generiert:

```Swift
let randomDouble = Double.random(in: 1...100)
print(randomDouble) // Output: 75.82
```

# Tiefergehende Informationen

Die in Swift verwendeten Methoden für die Generierung von Zufallszahlen basieren auf der "Mersenne Twister" Algorithmus. Dieser Algorithmus sorgt dafür, dass die generierten Zahlen wirklich zufällig sind und nicht durch eine gewisse Sequenz vorhersehbar werden.

Wenn man mehr Kontrolle über die Generierung von Zufallszahlen haben möchte, gibt es in Swift auch die Möglichkeit, die Klasse `RandomNumberGenerator` zu verwenden. Mit dieser können eigene Algorithmen zur Generierung von Zufallszahlen implementiert werden.

# Siehe auch

[Mersenne Twister Algorithmus](https://de.wikipedia.org/wiki/Mersenne-Twister)

[Offizielle Dokumentation zu Random in Swift](https://developer.apple.com/documentation/swift/random)

[Swift Tutorial: Zufallszahlen generieren](https://www.raywenderlich.com/17483599-swift-tutorial-random-numbers)