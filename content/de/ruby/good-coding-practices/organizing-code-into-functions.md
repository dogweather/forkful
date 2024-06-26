---
date: 2024-01-26 01:11:54.403673-07:00
description: "Wie geht das: Stellen Sie sich vor, Sie schreiben ein schnelles Skript,\
  \ um Benutzer zu begr\xFC\xDFen."
lastmod: '2024-03-13T22:44:54.405917-06:00'
model: gpt-4-1106-preview
summary: "Stellen Sie sich vor, Sie schreiben ein schnelles Skript, um Benutzer zu\
  \ begr\xFC\xDFen."
title: Code in Funktionen organisieren
weight: 18
---

## Wie geht das:
Stellen Sie sich vor, Sie schreiben ein schnelles Skript, um Benutzer zu begrüßen:

```Ruby
def greet(name)
  "Hallo, #{name}!"
end

puts greet("Alice")   # Ausgabe: Hallo, Alice!
puts greet("Bob")     # Ausgabe: Hallo, Bob!
```

Oder vielleicht berechnen Sie die Fläche eines Kreises:

```Ruby
def circle_area(radius)
  Math::PI * radius ** 2
end

puts circle_area(5)   # Ausgabe: 78.53981633974483
```

Sauberer und einfacher zu handhaben, richtig?

## Vertiefung
Das Konzept der Funktionen, auch als Methoden in Ruby bekannt, ist nicht neu – es ist so alt wie das Programmieren selbst. Zurückgehend auf die 1950er Jahre, wurden Unterprogramme, wie sie bekannt waren, eingeführt, um Redundanz zu reduzieren.

Alternativen? Sicher, Sie könnten Inline-Code verwenden, objektorientiert mit Klassen und Objekten arbeiten, oder sogar funktional mit Lambdas und Procs gehen. Aber Funktionen sind das A und O des geordneten Codes. Wollen Sie Leistung? Lokale Variablen in Funktionen sind schnell und Funktionen können Werte sofort mit `return` zurückgeben.

Umsetzungstechnisch können Sie eine Funktion mit `def` definieren und mit `end` beenden. Sie können Standardparameter festlegen, Splat-Operatoren für variadische Funktionen verwenden und mehr. Funktionen können so einfach oder komplex sein, wie Sie möchten.

## Siehe auch
- [Ruby's Methodendokumentation](https://ruby-doc.org/core-2.7.0/Method.html)
- [Learn to Program von Chris Pine](https://pine.fm/LearnToProgram/)
- [Practical Object-Oriented Design in Ruby von Sandi Metz](https://www.poodr.com/)
