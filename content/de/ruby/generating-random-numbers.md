---
title:                "Ruby: Erzeugung von Zufallszahlen"
simple_title:         "Erzeugung von Zufallszahlen"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/ruby/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Warum 

Möchtest du in deiner Ruby-Programmierung zufällige Zahlen generieren? Es gibt viele Anwendungen für die Verwendung von Zufallszahlen in deinem Code. Vielleicht möchtest du eine Lotterie- oder Quiz-Anwendung erstellen, oder einfach nur ein lustiges Spiel programmieren. Egal aus welchem ​​Grund, das Generieren von Zufallszahlen ist ein wichtiger Teil der Programmierung.

## Wie man es macht 

Um Zufallszahlen in Ruby zu erstellen, verwenden wir die `rand()`-Methode. Schau dir das Beispiel unten an, um zu sehen, wie es funktioniert:

```Ruby
# Generiert eine zufällige Ganzzahl zwischen 1 und 10 
puts rand(1..10) 

# Generiert eine zufällige Gleitkommazahl zwischen 0 und 1 
puts rand() 
```

Das Ergebnis dieser beiden `puts`-Anweisungen wird jedes Mal unterschiedlich sein, wenn du dein Programm ausführst. Dies ist der Grund, warum Zufallszahlen so nützlich sind - sie sind nicht vorhersehbar und können immer wieder neu generiert werden.

## Eine tiefere Eintauchen 

Die `rand()`-Methode verwendet einen Pseudozufallsgenerator, um Zufallszahlen zu erstellen. Dies bedeutet, dass die generierten Zahlen nicht wirklich zufällig sind, sondern auf einem Algorithmus basieren. Wenn du also dieselbe Anzahl von Zufallszahlen in der gleichen Reihenfolge benötigst, kannst du den Generator mit `srand()` und einer festen Zahl "initialisieren". Schau dir das Beispiel unten an:

```Ruby 
# Initialisiert den Pseudozufallsgenerator mit einer festen Zahl 
srand(12345) 
puts rand() # <-- Gleiche Zahl, da wir dieselbe Zahl für srand() verwenden 

puts rand() # <-- Gleiche Zahl, da wir den Generator nicht neu initialisiert haben 
```

Wie du sehen kannst, werden die ersten beiden Zahlen in beiden Ausführungen desselben Programms identisch sein, aber die dritte Zahl wird unterschiedlich sein, da wir den Generator nicht wieder initialisiert haben.

## Siehe auch 

- [Ruby-Dokumentation: Kernel#rand](https://ruby-doc.org/core-3.0.2/Kernel.html#method-i-rand)
- [Praktisches Beispiel für die Verwendung von Zufallszahlen in Ruby](https://www.rubyguides.com/2015/03/ruby-random/)
- [Das Geheimnis hinter Zufallszahlen in der Programmierung](https://www.freecodecamp.org/news/how-computer-generate-random-numbers/)