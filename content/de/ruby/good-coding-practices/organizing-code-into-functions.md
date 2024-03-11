---
date: 2024-01-26 01:11:54.403673-07:00
description: "Code in Funktionen zu organisieren teilt Ihr Skript in wiederverwendbare\
  \ Bl\xF6cke auf. Es geht darum, Ihren Code sauber, handhabbar und weniger\u2026"
lastmod: '2024-03-11T00:14:28.318678-06:00'
model: gpt-4-1106-preview
summary: "Code in Funktionen zu organisieren teilt Ihr Skript in wiederverwendbare\
  \ Bl\xF6cke auf. Es geht darum, Ihren Code sauber, handhabbar und weniger\u2026"
title: Code in Funktionen organisieren
---

{{< edit_this_page >}}

## Was & Warum?
Code in Funktionen zu organisieren teilt Ihr Skript in wiederverwendbare Blöcke auf. Es geht darum, Ihren Code sauber, handhabbar und weniger fehleranfällig zu machen. Modularer Code ist toll, weil er Zeit spart, den Verstand schont und das Debuggen sowie das Unit Testing vereinfacht.

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
