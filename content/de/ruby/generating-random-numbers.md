---
title:                "Erzeugung von Zufallszahlen"
html_title:           "Ruby: Erzeugung von Zufallszahlen"
simple_title:         "Erzeugung von Zufallszahlen"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/ruby/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Warum

Warum sollte man sich überhaupt mit der Generierung von Zufallszahlen beschäftigen? Nun, es gibt viele Anwendungen in der Programmierung, bei denen die Verwendung von Zufallszahlen erforderlich ist, zum Beispiel für Spiele, Simulationen oder Kryptographie.

# Wie geht's?

Um Zufallszahlen in Ruby zu generieren, können wir die `rand`-Methode verwenden. Diese Methode gibt eine zufällige Zahl zwischen 0 und 1 zurück. Schauen wir uns ein Beispiel an:

```Ruby
rand #=> 0.5910470116766848
```

Um eine zufällige Zahl in einem bestimmten Bereich zu erhalten, können wir die `rand`-Methode mit einem Bereichsoperator `..` verwenden. Zum Beispiel möchten wir eine zufällige Zahl zwischen 1 und 10 erhalten:

```Ruby
rand(1..10) #=> 9
```

Wir können auch mehrere Zufallszahlen auf einmal generieren, indem wir die `rand`-Methode mit einem Array verwenden. Zum Beispiel möchten wir 3 zufällige Zahlen zwischen 1 und 6 erhalten:

```Ruby
rand(1..6, 3) #=> [2, 5, 1]
```

Es gibt viele weitere Möglichkeiten, Zufallszahlen in Ruby zu generieren, zum Beispiel mit den Methoden `randF` für Fließkommazahlen und `random_number` für Ganzzahlen.

# Tiefer eintauchen

Die `rand`-Methode in Ruby verwendet einen Algorithmus namens Mersenne Twister, um Zufallszahlen zu generieren. Dabei handelt es sich um einen der schnellsten und am besten untersuchten Zufallszahlengeneratoren. Dieser Algorithmus basiert auf mathematischen Berechnungen, um scheinbar zufällige Zahlen zu erzeugen.

Eine wichtige Sache, die man bei der Verwendung von `rand` in Ruby beachten sollte, ist, dass die generierten Zahlen nicht wirklich zufällig sind, sondern vorhersagbar. Sie können nur eine Illusion von Zufälligkeit erzeugen, die für viele Anwendungen ausreichend ist, aber nicht für kryptographische Zwecke.

# Siehe auch

- Die offizielle Dokumentation zu `rand`: https://ruby-doc.org/core-3.0.0/Random.html
- Ein Rückschritt zum Mersenne Twister-Zufallszahlengenerator: https://ruby-doc.org/stdlib-3.0.0/libdoc/securerandom/rdoc/Random/MersenneTwister.html
- Ein Artikel über die Verwendung von Zufallszahlen in der Programmierung: https://www.thoughtco.com/what-are-random-numbers-958074