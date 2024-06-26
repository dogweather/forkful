---
date: 2024-01-20 17:56:40.695084-07:00
description: "Wie geht das? Ruby macht es einfach, auf Befehlszeilenargumente zuzugreifen\
  \ \u2013 sie werden in einem speziellen Array namens `ARGV` gespeichert."
lastmod: '2024-04-05T22:38:55.288941-06:00'
model: gpt-4-1106-preview
summary: "Wie geht das? Ruby macht es einfach, auf Befehlszeilenargumente zuzugreifen\
  \ \u2013 sie werden in einem speziellen Array namens `ARGV` gespeichert."
title: Lesen von Kommandozeilenargumenten
weight: 23
---

## Wie geht das?
Ruby macht es einfach, auf Befehlszeilenargumente zuzugreifen – sie werden in einem speziellen Array namens `ARGV` gespeichert.

```ruby
# script.rb
puts "Anzahl der Argumente: #{ARGV.length}"
puts "Die Argumente sind: #{ARGV.join(', ')}"

# Ausführen mit: ruby script.rb diese sind test daten
# Erwartete Ausgabe:
# Anzahl der Argumente: 4
# Die Argumente sind: diese, sind, test, daten
```

Kurz und knapp, `ARGV` enthält die Werte, fertig. Das war's eigentlich schon.

## Deep Dive
In älteren Zeiten hatten Programmierer es nicht so einfach. Sie mussten komplizierte Wege gehen, um Zugriff auf Befehlszeilenargumente zu bekommen. In Ruby hingegen ist die Standardbibliothek seit jeher unser Freund.

Alternativen zu `ARGV` gibt es auch: Die `OptionParser`-Bibliothek zum Beispiel hilft, wenn es um komplexere Aufgaben geht, etwa wenn man Argumente mit Schaltern (Flags) braucht oder verschiedene Optionen hat.

Intern speichert Ruby die Argumente, die du in deiner Konsole eingibst, in dem globalen Array `ARGV`, bevor das Skript überhaupt startet. Die Argumente werden als Strings behandelt, also denk dran, sie umzuwandeln, falls du Zahlen oder ähnliches brauchst.

```ruby
# argument_conversion.rb
number_arguments = ARGV.map(&:to_i)
puts "Zahlen addiert: #{number_arguments.reduce(:+)}"

# Ausführen mit: ruby argument_conversion.rb 10 20 30
# Erwartete Ausgabe:
# Zahlen addiert: 60
```

Hier kam die `map`-Methode zum Zug, um jedes Argument in eine Zahl umzuwandeln, und `reduce`, um diese zu addieren.

## Siehe auch
- Ruby-Docs über ARGV: [https://ruby-doc.org/core-2.7.0/ARGF.html](https://ruby-doc.org/core-2.7.0/ARGF.html)
- Anleitung zu `OptionParser`: [https://ruby-doc.org/stdlib-2.7.0/libdoc/optparse/rdoc/OptionParser.html](https://ruby-doc.org/stdlib-2.7.0/libdoc/optparse/rdoc/OptionParser.html)
