---
title:                "Die Länge einer Zeichenkette bestimmen"
html_title:           "Ruby: Die Länge einer Zeichenkette bestimmen"
simple_title:         "Die Länge einer Zeichenkette bestimmen"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/ruby/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?

Die Länge einer Zeichenkette zu finden bedeutet, die Anzahl der Zeichen in einem Text oder einer Folge von Buchstaben zu ermitteln. Dies ist nützlich, um die Genauigkeit von Eingaben zu überprüfen oder um die Position bestimmter Zeichen innerhalb einer Zeichenkette zu bestimmen. Programmierer nutzen diese Funktion, um die korrekte Verarbeitung von Benutzereingaben sicherzustellen und um effizient auf spezifische Zeichen zuzugreifen.

## Wie geht's:

```
# Beispiel 1
puts "Hallo, Welt!".length
# Output: 12

# Beispiel 2
name = "Maria"
puts name.length
# Output: 5
```

## Tiefergehende Information:

(1) Die Funktion `length` existiert nicht nur in der aktuellen Version von Ruby, sondern wird auch in älteren Versionen und anderen Programmiersprachen wie Java oder JavaScript verwendet. (2) Eine alternative Methode, um die Länge einer Zeichenkette zu ermitteln, ist die Verwendung von Regular Expressions oder die manuelle Zählung der Zeichen. (3) In Ruby wird die Länge einer Zeichenkette durch eine interne Zählschleife ermittelt, was eine schnelle und effiziente Methode darstellt.

## Siehe auch:

- [Ruby-Dokumentation zur Funktion `length`](https://ruby-doc.org/core-3.0.2/String.html#method-i-length)
- [Beispiele zur Nutzung von `length` in Ruby](https://www.rubyguides.com/2018/10/ruby-string-length/)
- [Alternative Methoden zur Ermittlung der Länge einer Zeichenkette in Ruby](https://www.rubyguides.com/2018/06/ruby-string-methods/#length)