---
title:                "Lesen von Befehlszeilenargumenten"
html_title:           "Ruby: Lesen von Befehlszeilenargumenten"
simple_title:         "Lesen von Befehlszeilenargumenten"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/ruby/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Warum

Warum sollte man sich mit dem Lesen von Befehlszeilenargumenten beschäftigen? Nun, es ist eine wichtige Fähigkeit, um die Eingabe von Benutzern in einem Ruby-Programm zu verarbeiten und es flexibel und interaktiv zu gestalten.

## Wie funktioniert es?

Das Lesen von Befehlszeilenargumenten in Ruby ist relativ einfach und kann mit der "ARGV" Variable durchgeführt werden. Diese Variable enthält alle Argumente, die bei der Ausführung des Programms angegeben wurden.

```Ruby
# Beispielprogramm, das die ersten beiden Argumente ausgibt

puts ARGV[0]
puts ARGV[1]
```

Wenn wir dieses Programm mit den Argumenten "Hallo" und "Welt" ausführen würden, wäre die Ausgabe:

```
Hallo
Welt
```

Zusätzlich können wir auch eine Schleife verwenden, um alle Argumente auszugeben:

```Ruby
# Beispielprogramm, das alle Argumente ausgibt

ARGV.each do |arg|
  puts arg
end
```

Bei der Ausführung mit den Argumenten "Ruby", "ist" und "super!" wäre die Ausgabe:

```
Ruby
ist
super!
```

## Tiefer Einblick

Während einfaches Lesen von Befehlszeilenargumenten in den meisten Fällen ausreicht, gibt es Situationen, in denen mehr Kontrolle erforderlich ist. In solchen Fällen können Option-Parsing-Libraries wie "optparse" oder "docopt" verwendet werden, die es ermöglichen, Argumente mit bestimmten Formatierungen oder Optionen zu verarbeiten.

Ein weiterer wichtiger Aspekt ist die Validierung der Eingabe der Benutzer. Dies kann durch Überprüfen von Argumenten auf bestimmte Bedingungen oder durch Verwendung von Standard-Argumenten erreicht werden, falls keine Argumente angegeben werden.

## Sieh auch

- Offizielle Ruby-Dokumentation zu Befehlszeilenargumenten: https://ruby-doc.org/core-3.0.0/ARGF.html
- Eine Einführung in Option-Parsing in Ruby: https://www.rubyguides.com/2018/08/ruby-optionparser/
- Eine Übersicht über "docopt": https://github.com/docopt/docopt.rb