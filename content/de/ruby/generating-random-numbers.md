---
title:                "Ruby: Eineinerung von Zufallszahlen"
programming_language: "Ruby"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/ruby/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Warum

Generieren von zufälligen Zahlen kann nützlich sein, um beispielsweise Passwörter oder Geheimzahlen zu erstellen oder um in Simulationen oder Spielen zufällige Elemente zu erzeugen.

## Wie man es macht

Es gibt verschiedene Möglichkeiten, in Ruby zufällige Zahlen zu generieren. Eine Möglichkeit ist die Verwendung der Methode `.rand()` mit einem optionalen Argument, um den Bereich der Zahlen anzugeben, aus denen die zufällige Zahl gewählt werden soll.

```Ruby
# Generieren einer zufälligen Zahl zwischen 0 und 10
puts rand(10)
# Beispiel Output: 5

# Generieren einer zufälligen Fließkommazahl zwischen 0 und 5
puts rand(0.0..5.0)
# Beispiel Output: 3.278948935
```

Eine weitere Möglichkeit ist die Verwendung der Klasse `Random`, die mehr Kontrolle über die Generierung zufälliger Zahlen bietet.

```Ruby
# Erstellen einer neuen Instanz von Random
random = Random.new

# Generieren einer zufälligen Zahl zwischen 0 und 100
puts random.rand(100)
# Beispiel Output: 47

# Generieren einer zufälligen Zahl zwischen -5 und 5
puts random.rand(-5..5)
# Beispiel Output: -3
```

Es ist auch möglich, zufällige Elemente aus einem Array auszuwählen.

```Ruby
# Erstellen eines Arrays
fruit = ["Apfel", "Banane", "Orange", "Erdbeere", "Kiwi"]

# Zufällige Auswahl eines Elementes aus dem Array
puts fruit.sample
# Beispiel Output: Erdbeere
```

## Tieferer Einblick

Zufallszahlen können auf verschiedene Weise generiert werden, aber es gibt wichtige Faktoren zu beachten. Zum Beispiel kann das Einbinden der aktuellen Zeit in den Algorithmus dazu führen, dass die generierten Zahlen nicht tatsächlich zufällig sind, da die Zeit sich wiederholen kann. Auch die Verwendung von bestimmten Basiszahlen oder geheimen Mustern kann das Ergebnis weniger zufällig machen. Es ist wichtig, sorgfältig zu wählen, welche Methode zur Generierung zufälliger Zahlen verwendet wird, je nachdem, für welchen Zweck sie benötigt werden.

## Siehe auch

- [Ruby Dokumentation: rand()](https://ruby-doc.org/core-2.6.3/Kernel.html#method-i-rand)
- [Ruby Dokumentation: Random](https://ruby-doc.org/core-2.6.3/Random.html)
- [Ruby Dokumentation: Array#sample](https://ruby-doc.org/core-2.6.3/Array.html#method-i-sample)