---
title:                "Generierung von Zufallszahlen"
date:                  2024-01-20T17:49:46.270527-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generierung von Zufallszahlen"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/ruby/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why?
Zufallszahlen zu generieren bedeutet, eine Zahl zu erzeugen, die für einen Außenstehenden nicht vorhersehbar ist. Programmierer nutzen das für Sicherheitsfeatures, Spiele, Simulationen und Tests, um nur einige Anwendungsfälle zu nennen.

## How to:
Ruby macht es einfach, Zufallszahlen zu generieren. Hier einige Beispiele:

```Ruby
# Erzeugen einer Zufallszahl zwischen 0 und 1
puts rand # => 0.543775527487475

# Erzeugen einer ganzen Zufallszahl zwischen 0 und 10 
puts rand(11) # => 7

# Erzeugen einer Zufallszahl zwischen einem Bereich von 50 bis 100
puts rand(50..100) # => 85

# Erzeugen einer Zufallszahl mit dem Random-Objekt für mehr Kontrolle
prng = Random.new
puts prng.rand(100) # => 44
```

## Deep Dive
Die Methode, Zufallszahlen zu generieren, hat sich über die Jahre entwickelt. Früher nutzte man physische Objekte wie Würfel oder Roulette-Räder. In Computerprogrammen nutzt man Algorithmen, die 'Pseudozufallszahlen' generieren. Diese sind vorhersehbar, wenn man den Anfangswert (Seed) kennt. Ruby nutzt den Mersenne-Twister-Algorithmus für hohe Qualität der Zufallszahlen. Für kryptografische Sicherheit gibt es `SecureRandom` mit stärkeren Zufallszahlen.

Alternativen zur `rand`-Methode sind:

- `Random.rand`: Identische Funktion zu `rand`, aber klarer als Klassenmethode definiert.
- `SecureRandom`: Für Kryptographie, da es auf dem OS basiert um echte Zufallszahlen zu produzieren.

In praktischen Anwendungen ist es oft nötig, den Seed manuell zu setzen. Das ist wichtig für reproduzierbare Tests und Simulationen:

```Ruby
Random.srand(1234)
puts Random.rand # => Immer gleiche Zahl bei gleichem Seed
```

## See Also
Um mehr zu erfahren, schau in die Ruby-Dokumentation:

- Allgemeine Ruby-Dokumentation: [https://www.ruby-lang.org/de/documentation/](https://www.ruby-lang.org/de/documentation/)
- Ruby's Random-Klasse: [https://ruby-doc.org/core-3.1.2/Random.html](https://ruby-doc.org/core-3.1.2/Random.html)
- SecureRandom-Bibliothek: [https://ruby-doc.org/stdlib-3.1.2/libdoc/securerandom/rdoc/SecureRandom.html](https://ruby-doc.org/stdlib-3.1.2/libdoc/securerandom/rdoc/SecureRandom.html)
- Über den Mersenne-Twister: [https://de.wikipedia.org/wiki/Mersenne-Twister](https://de.wikipedia.org/wiki/Mersenne-Twister)
