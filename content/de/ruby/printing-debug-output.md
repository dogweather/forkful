---
title:                "Ausgabe von Debugging-Informationen drucken"
html_title:           "Bash: Ausgabe von Debugging-Informationen drucken"
simple_title:         "Ausgabe von Debugging-Informationen drucken"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/ruby/printing-debug-output.md"
---

{{< edit_this_page >}}

## Was und Warum?

Drucken von Debug-Ausgaben bezeichnet das protokollieren von Programmabläufen zur Fehleranalyse. Dies ist nützlich, um Fehlerursachen bei der Ausführung des Programms zu ermitteln und zu beheben.

## "So geht's":

```Ruby
while i < 10
  puts "Wir befinden uns in der Iteration #{i}"
  i += 1
end
```
In diesem Codeblock wird in jeder Iteration eine Debug-Ausgabe generiert, die den Moment in der Schleife anzeigt.

Ausgabe:
```
Wir befinden uns in der Iteration 0
Wir befinden uns in der Iteration 1
Wir befinden uns in der Iteration 2
...
```

## Deep Dive:

Die Verwendung von Debug-Ausgabe hat ihre Wurzeln in der frühen Geschichte der Programmierung, wo physische Löcher in Lochkartenbändern ausgegeben wurden, um Daten und Code darzustellen.
Alternativen zur direkten Ausgabe von Debug-Informationen könnten beispielsweise Unit-Tests oder formale Verifikation sein.
Unter der Haube wird puts in Ruby durch die C-schichtige IO#write Methode implementiert.

## Siehe auch:

Für weitergehende Informationen sind folgende Quellen empfehlenswert:

- [Ruby Docs: IO](https://ruby-doc.org/core-2.7.0/IO.html)
- [Practical Debugging in Ruby](https://medium.com/@kevinsimper/debugging-in-ruby-byebug-7e6f8a9a19a1)