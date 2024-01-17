---
title:                "Zufällige Zahlen erzeugen"
html_title:           "Ruby: Zufällige Zahlen erzeugen"
simple_title:         "Zufällige Zahlen erzeugen"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/ruby/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Was & Warum?

Generieren von Zufallszahlen ist ein grundlegender Teil des Programmierens. Es bezieht sich auf die Erstellung von beliebigen Zahlen innerhalb eines bestimmten Bereichs. Programmierer tun dies, um viele verschiedene Dinge zu erreichen, einschließlich der Simulation realer Ereignisse, der Erstellung von Spielen und der Verschlüsselung von Daten.

# Wie geht's?

Das Erzeugen von Zufallszahlen in Ruby ist einfach. Um Zahlen innerhalb eines bestimmten Bereichs zu generieren, verwenden Sie die Methode `rand`, gefolgt von einem Intervall. Zum Beispiel generiert `rand(1..10)` eine Zufallszahl zwischen 1 und 10.

```Ruby
# Generiere eine Zufallszahl zwischen 1 und 10
puts rand(1..10) # Beispiel Ausgabe: 7
```

Sie können auch eine Zufallszahl ohne angegebenen Bereich generieren, indem Sie die Methode `rand` alleine verwenden. Dies generiert eine beliebige Zahl zwischen 0 und 1.

```Ruby
# Generiere eine Zufallszahl zwischen 0 und 1
puts rand # Beispiel Ausgabe: 0.548653
```

# Tiefergehende Information

Zufallszahlen sind seit langem ein wichtiges Konzept in der Informatik. Sie werden für verschiedene Zwecke verwendet, einschließlich der Modellierung von Wahrscheinlichkeit in Algorithmen und Simulationen. Es gibt auch alternative Methoden zum Generieren von Zufallszahlen, wie zum Beispiel die Verwendung von externen Quellen wie physikalischen Geräten oder Online-Zufallszahlengeneratoren.

In Ruby wird das Erzeugen von Zufallszahlen durch eine pseudozufällige Generierungsfunktion erreicht, die auf dem Mersenne Twister-Algorithmus basiert. Dieser Algorithmus ist in der Lage, eine lange Sequenz von Zahlen zu generieren, die scheinbar zufällig sind, aber auf einer festgelegten Startzahl basieren.

# Weitere Informationen

Um mehr über das Erzeugen von Zufallszahlen in Ruby zu erfahren, besuchen Sie die offizielle Dokumentation: https://ruby-doc.com/core-2.7.1/Kernel.html#method-i-rand

Für andere Möglichkeiten, Zufallszahlen zu erzeugen, können Sie sich auch den Zufallszahlengenerator-Bibliotheken für Ruby ansehen: https://github.com/seattlerb/mersenne_twister oder https://rubygems.org/gems/random_number_generator