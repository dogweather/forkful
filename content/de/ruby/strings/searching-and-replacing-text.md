---
date: 2024-01-20 17:58:44.690394-07:00
description: "How to: Urspr\xFCnglich stammt die Idee des Suchens und Ersetzens aus\
  \ der Textverarbeitung und hat ihren Weg in die Programmierung gefunden. Fr\xFC\
  he Editoren\u2026"
lastmod: '2024-04-05T21:53:56.279980-06:00'
model: gpt-4-1106-preview
summary: "Urspr\xFCnglich stammt die Idee des Suchens und Ersetzens aus der Textverarbeitung\
  \ und hat ihren Weg in die Programmierung gefunden."
title: Suchen und Ersetzen von Text
weight: 10
---

## How to:
```Ruby
text = "Hallo Welt! Hallo Programmierung!"

# Einfacher Ersatz
ersetzter_text = text.gsub('Hallo', 'Tschüss')
puts ersetzter_text  # "Tschüss Welt! Tschüss Programmierung!"

# Mit Regex (Reguläre Ausdrücke)
regex_ersetzter_text = text.gsub(/H.llo/, 'Servus')
puts regex_ersetzter_text  # "Servus Welt! Servus Programmierung!"

# Block-Form, um dynamisch zu ersetzen
nummeriert_text = text.gsub(/Hallo/) { |match| "Nummerierte Begrüßung #{match.downcase}" }
puts nummeriert_text  # "Nummerierte Begrüßung hallo Welt! Nummerierte Begrüßung hallo Programmierung!"
```

## Deep Dive
Ursprünglich stammt die Idee des Suchens und Ersetzens aus der Textverarbeitung und hat ihren Weg in die Programmierung gefunden. Frühe Editoren wie `ed` und `vi` in Unix ermöglichten es bereits, aber in Ruby macht uns die `gsub`-Methode (global substitute) das Leben leichter. Alternativen außerhalb von Ruby sind beispielsweise `sed` in Unix oder Suchen-Ersetzen-Funktionen in modernen Editoren wie `VSCode`. 

Intern implementiert Ruby `gsub` mit leistungsfähigen Regulären Ausdrücken, welche es ermöglichen, komplexe Suchmuster zu definieren und die Leistung beim Durchsuchen des Textes zu optimieren.

## See Also
- Ruby-Dokumentation zu `String#gsub` und `String#sub`: [https://ruby-doc.org/core-3.1.1/String.html#method-i-gsub](https://ruby-doc.org/core-3.1.1/String.html#method-i-gsub)
- Einführung in reguläre Ausdrücke in Ruby: [https://www.rubyguides.com/2015/06/ruby-regex/](https://www.rubyguides.com/2015/06/ruby-regex/)
- Interaktives Lernen von Regex mit Ruby: [https://rubular.com/](https://rubular.com/)
