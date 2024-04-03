---
date: 2024-01-26 03:36:52.343960-07:00
description: "Refactoring ist der Prozess der Umstrukturierung von bestehendem Computercode,\
  \ ohne dessen externes Verhalten zu \xE4ndern. Programmierer f\xFChren ein\u2026"
lastmod: '2024-03-13T22:44:54.408707-06:00'
model: gpt-4-0125-preview
summary: "Refactoring ist der Prozess der Umstrukturierung von bestehendem Computercode,\
  \ ohne dessen externes Verhalten zu \xE4ndern."
title: Refactoring
weight: 19
---

## Was & Warum?

Refactoring ist der Prozess der Umstrukturierung von bestehendem Computercode, ohne dessen externes Verhalten zu ändern. Programmierer führen ein Refactoring durch, um nichtfunktionale Attribute der Software zu verbessern, wie etwa Lesbarkeit, reduzierte Komplexität, verbesserte Wartbarkeit oder Leistungssteigerung.

## Wie geht das:

Lassen Sie uns anhand eines Beispiels das Refactoring einer Ruby-Methode durchgehen, die die Summe der Quadrate berechnet.

**Vor dem Refactoring:**
```ruby
def sum_of_squares(numbers)
  sum = 0
  numbers.each do |number|
    square = number * number
    sum += square
  end
  sum
end

puts sum_of_squares([1, 2, 3])  # Ausgabe: 14
```

**Nach dem Refactoring:**
```ruby
def sum_of_squares(numbers)
  numbers.map { |number| number**2 }.sum
end

puts sum_of_squares([1, 2, 3])  # Ausgabe: 14
```

Die überarbeitete Version verwendet Ruby Enumerables, um dieselbe Logik prägnanter und klarer auszudrücken. Die `map`-Methode transformiert jedes Element und `sum` aggregiert ihre Werte, wodurch die Notwendigkeit für manuelle Schleifenverwaltung und Variablenzuweisung entfällt.

## Tiefere Einblicke

Refactoring hat einen reichen historischen Kontext, der bis zu den frühen Praktiken in der Softwareentwicklung zurückreicht. Erste Erwähnungen lassen sich bis in die 1990er Jahre zurückverfolgen, mit signifikanten Beiträgen von Martin Fowler in seinem Buch "Refactoring: Improving the Design of Existing Code", in dem er einen Katalog von Mustern für das Refactoring bereitstellt. Seitdem ist das Refactoring zu einem Eckpfeiler der agilen Entwicklungspraktiken geworden.

Wenn wir über Alternativen zum Refactoring sprechen, müssen wir entweder einen anderen Ansatz wie „Neuschreiben“ in Betracht ziehen, bei dem Sie das alte System teilweise oder ganz ersetzen, oder Praktiken wie „Code Reviews“ und „Pair Programming“ anwenden, um die Codequalität schrittweise zu verbessern. Jedoch sind diese keine Ersatz für Refactoring; sie ergänzen den Prozess.

Hinsichtlich der Implementierung bietet Ruby eine ausgezeichnete und ausdrucksstarke Syntax, die oft zu kürzerem, lesbarerem Code nach einem Refactoring führt. Schlüsselprinzipien sind DRY (Don't Repeat Yourself), die Verwendung sinnvoller Namen, das Halten der Methoden kurz und fokussiert auf eine einzelne Aufgabe und die effektive Nutzung des Enumerable-Moduls von Ruby, wie im obigen Beispiel zu sehen. Automatisierte Tools wie RuboCop können Programmierern auch helfen, Stellen im Code zu identifizieren, die von einem Refactoring profitieren könnten.

## Siehe auch

Um tiefer in das Refactoring in Ruby einzutauchen, prüfen Sie diese Ressourcen:

- Das richtungsweisende Buch von Martin Fowler: [Refactoring: Improving the Design of Existing Code](https://martinfowler.com/books/refactoring.html)
- Der Ruby-Stilführer für sauberen Code: [The Ruby Style Guide](https://rubystyle.guide/)
- RuboCop, ein statischer Codeanalysator (Linter) und Formatierer: [RuboCop GitHub Repository](https://github.com/rubocop/rubocop)
