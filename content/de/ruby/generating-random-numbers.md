---
title:    "Ruby: Generierung zufälliger Zahlen"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Warum
Als Programmierer hat man oft eine Aufgabe oder ein Problem, bei dem es hilfreich ist, zufällige Zahlen zu generieren. Dies kann zum Beispiel bei einer Simulation oder beim Testen von Funktionen nützlich sein. In diesem Blog-Beitrag lernen Sie, wie Sie in Ruby zufällige Zahlen generieren können.

## Wie man Zufall in Ruby verwendet
Um zufällige Zahlen in Ruby zu erzeugen, müssen Sie zunächst das "random" Modul importieren. Dieses Modul enthält alle nötigen Funktionen für die Erzeugung von Zufallszahlen.

```Ruby
require 'random'
```

Als nächstes können Sie mit der `rand` Funktion eine zufällige Zahl im Bereich von 0 bis 1 erzeugen.

```Ruby
puts rand # Output: 0.242426
```

Um eine zufällige Ganzzahl im Bereich von 1 bis 10 zu erzeugen, können Sie die `rand` Funktion mit einer Range verwenden.

```Ruby
puts rand(1..10) # Output: 8
```

Sie können auch die `rand` Funktion mehrmals aufrufen, um mehrere Zufallszahlen zu generieren.

```Ruby
puts rand(1..10) # Output: 6
puts rand(1..10) # Output: 2
puts rand(1..10) # Output: 9
```

Wenn Sie eine zufällige Zahl mit Nachkommastellen benötigen, können Sie die `rand` Funktion mit einer Gleitkommazahl multiplizieren.

```Ruby
puts rand(1.0..10.0) # Output: 3.872
```

Um eine zufällige Zahl aus einer bestimmten Liste auszuwählen, können Sie die `sample` Funktion verwenden.

```Ruby
liste = [1, 2, 3, 4, 5]
puts liste.sample # Output: 4
```

## Tiefergehende Informationen
Wenn Sie sich für die Details der Zufallsgenerierung in Ruby interessieren, können Sie sich die Dokumentation des "random" Moduls ansehen. Dort finden Sie Funktionen wie `seed` und `srand`, mit denen Sie den generierten Zufallszahlen eine bestimmte Ausgangsbasis geben können.

Darüber hinaus bietet Ruby auch das "SecureRandom" Modul an, das für Anwendungen verwendet werden sollte, bei denen Sicherheit von höchster Priorität ist, wie zum Beispiel beim Generieren von Passwörtern.

## Siehe auch
- [Ruby Dokumentation zum "random" Modul](https://ruby-doc.org/core-3.0.0/Random.html)
- [Ruby Dokumentation zum "SecureRandom" Modul](https://ruby-doc.org/stdlib-3.0.0/libdoc/securerandom/rdoc/SecureRandom.html)