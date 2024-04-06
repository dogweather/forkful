---
date: 2024-01-26 03:46:56.837698-07:00
description: "Wie: Das Runden von Zahlen ist nicht neu \u2013 Menschen tun dies seit\
  \ Jahrhunderten, um Berechnungen zu vereinfachen oder um innerhalb der Grenzen ihrer\u2026"
lastmod: '2024-04-05T22:51:08.928984-06:00'
model: gpt-4-0125-preview
summary: "Das Runden von Zahlen ist nicht neu \u2013 Menschen tun dies seit Jahrhunderten,\
  \ um Berechnungen zu vereinfachen oder um innerhalb der Grenzen ihrer Werkzeuge\
  \ zu arbeiten."
title: Zahlen runden
weight: 13
---

## Wie:
```Ruby
# Grundlegendes Runden
puts 3.14159.round      # => 3
puts 2.6.round          # => 3

# Präzision angeben
puts 3.14159.round(2)   # => 3.14
puts 2.675.round(2)     # => 2.68

# Abwärts runden
puts 2.9.floor          # => 2

# Aufwärts runden
puts 2.1.ceil           # => 3

# Gegen Null runden
puts -2.9.round         # => -3
puts -2.9.truncate      # => -2
```

Beispielausgabe:
```
3
3
3.14
2.68
2
3
-3
-2
```

## Tiefergehend
Das Runden von Zahlen ist nicht neu – Menschen tun dies seit Jahrhunderten, um Berechnungen zu vereinfachen oder um innerhalb der Grenzen ihrer Werkzeuge zu arbeiten. In Ruby ist die `round` Methode vielseitig, mit der Fähigkeit, standardmäßig zur nächsten ganzen Zahl oder zu einer bestimmten Dezimalstelle zu runden.

Eine Alternative zu `round` ist `floor` für das immerwährende Abrunden und `ceil` für das immerwährende Aufrunden, unabhängig vom Wert der Zahl. Um einfach die Dezimalstellen abzuschneiden, gibt es `truncate`.

In der Geschichte, wenn es um Computer geht, wird das Runden kritisch im Umgang mit Fließkommaarithmetik aufgrund ihrer inhärenten Ungenauigkeit. Ruby, wie die meisten Sprachen, folgt dem IEEE 754-Standard für Fließkommazahlen, was bedeutet, dass es das Runden auf eine Weise handhabt, die die meisten Programmierer vorhersagen und darauf vertrauen sollten.

Es gibt jedoch mehr zu beachten – Dinge wie das Bankers Rounding (auch als Runde zur Halbgeraden bekannt) sind Konzepte, die Ruby-Entwickler möglicherweise manuell implementieren müssen, da die `round` Methode es nicht standardmäßig anbietet.

## Siehe auch
- Die [Ruby-Dokumentation](https://ruby-doc.org/core-3.0.0/Float.html#method-i-round) für die `round` Methode von Floats.
- [IEEE-Standard für Fließkommaarithmetik (IEEE 754)](https://ieeexplore.ieee.org/document/4610935).
- [Verständnis der Fließkommagenauigkeit](https://floating-point-gui.de/), für einen tieferen Einblick, wie Computer Dezimalzahlen handhaben.
