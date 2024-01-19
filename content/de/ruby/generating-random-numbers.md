---
title:                "Zufallszahlen generieren"
html_title:           "Arduino: Zufallszahlen generieren"
simple_title:         "Zufallszahlen generieren"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/ruby/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Was & Warum?

Zufallszahlen-Generierung ist ein Prozess, bei dem eine Reihe von Zahlen erzeugt wird, die keine erkennbare Muster oder Sequenz aufweisen. Programmierer nutzen dies häufig für Funktionen wie Verschlüsselung, Computerspiele, oder zur Simulation von realen Szenarien.

## So geht's:

Hier eine kurze Anleitung, wie man in Ruby eine Zufallszahl generiert.

```ruby
puts rand(100)
```

Wenn Sie das obige Programm ausführen, erhalten Sie eine Zufallszahl zwischen 0 und 99.

## Tiefere Einblicke

### Historischer Kontext

Der Entwickler von Ruby, Yukihiro "Matz" Matsumoto, wählte den Mersenne-Twister-Algorithmus als Basis für die Erzeugung von Zufallszahlen in Ruby, da dieser eine hohe Qualität der Zufälligkeit bietet.

### Alternativen

Es gibt auch andere Methoden zur Generierung von Zufallszahlen in Ruby, wie zum Beispiel `Random.new`. Außerdem können Sie `SecureRandom`-Methoden wie `SecureRandom.random_number` nutzen, wenn Sie eine sicherere Möglichkeit zur Zufallsgenerierung benötigen.

### Implementierungsdetails

Der `rand`-Methode in Ruby kann ein Argument übergeben werden. Wenn kein Argument angegeben ist, gibt `rand` eine Fließkommazahl zurück, die größer oder gleich 0.0 und kleiner als 1.0 ist. Wenn Sie `rand(n)` aufrufen, erhalten Sie eine ganze Zufallszahl größer oder gleich 0 und kleiner als n.

## Siehe auch

- [Ruby Documentation: Kernel#rand](https://docs.ruby-lang.org/en/3.0.0/Kernel.html#method-i-rand)
- [Ruby Doc: Class: Random](https://docs.ruby-lang.org/en/3.0.0/Random.html)
- [Ruby Doc: Class: SecureRandom](https://docs.ruby-lang.org/en/3.0.0/SecureRandom.html)

Sie werden hier weitere nützliche Informationen zum Thema Zufallszahlen in Ruby finden.