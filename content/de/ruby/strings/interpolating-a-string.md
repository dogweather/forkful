---
date: 2024-01-20 17:51:37.573979-07:00
description: "String-Interpolation in Ruby erm\xF6glicht es, Variablenwerte innerhalb\
  \ eines Strings einzusetzen. Es macht den Code lesbarer und schafft dynamische\u2026"
lastmod: '2024-03-13T22:44:54.384153-06:00'
model: gpt-4-1106-preview
summary: "String-Interpolation in Ruby erm\xF6glicht es, Variablenwerte innerhalb\
  \ eines Strings einzusetzen."
title: Zeichenketten interpolieren
weight: 8
---

## What & Why? (Was & Warum?)
String-Interpolation in Ruby ermöglicht es, Variablenwerte innerhalb eines Strings einzusetzen. Es macht den Code lesbarer und schafft dynamische Textausgaben mit weniger Aufwand.

## How to: (Wie geht das:)
```Ruby
name = "Welt"
greeting = "Hallo #{name}!" # String-Interpolation mit #{...}
puts greeting # Ausgabe: Hallo Welt!

age = 25
message = "Ich bin #{age} Jahre alt." # Integers funktionieren auch
puts message # Ausgabe: Ich bin 25 Jahre alt.

price = 4.5
item = "Kaffee"
receipt = "Ein #{item} kostet #{'%.2f' % price} Euro." # Formatierung
puts receipt # Ausgabe: Ein Kaffee kostet 4.50 Euro.
```

## Deep Dive (Tiefer eintauchen)
Die String-Interpolation in Ruby existiert seit den ersten Versionen und ist eine verbreitete Methode, um Strings zu bearbeiten. Historisch setzte man häufig die `sprintf`-Methode oder deren Alias `format` ein, die nach wie vor gültige Alternativen sind.

```Ruby
# Alternative Methode mit `sprintf`
formatted_string = sprintf("Hello %s!", name)
puts formatted_string # Ausgabe: Hello Welt!

# Alternative Methode mit `format`
formatted_string = format("Kosten: %0.2f Euro", price)
puts formatted_string # Ausgabe: Kosten: 4.50 Euro
```

Es ist zu beachten, dass nur innerhalb doppelter Anführungszeichen ("") oder Backticks (`) String-Interpolation möglich ist. Einfache Anführungszeichen ('') interpretieren #{...} als normalen Text.

Die Interpolation ruft automatisch die `to_s`-Methode der Variable auf, wodurch fast jeder Objekttyp eingefügt werden kann. Im Falle von Objekten kann man diese Methode überschreiben, um die Ausgabe anzupassen.

## See Also (Siehe auch)
- Die offizielle Ruby-Dokumentation zur String-Interpolation: [Ruby Docs: String](https://ruby-doc.org/core-3.1.0/String.html#method-i-2B)
- Ein Ruby Tutorial zu Strings und Interpolation: [RubyLearning.com](http://rubylearning.com/satishtalim/ruby_strings.html)
- Mehr zu `sprintf` und `format`: [Kernel::sprintf](https://ruby-doc.org/core-3.1.0/Kernel.html#method-i-sprintf)
