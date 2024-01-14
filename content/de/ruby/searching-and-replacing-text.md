---
title:                "Ruby: Suchen und Ersetzen von Text"
simple_title:         "Suchen und Ersetzen von Text"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/ruby/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Warum
Text-Suche und -Ersetzung sind grundlegende Aufgaben in der Programmierung. Egal ob du falsche Schreibweisen korrigieren, Platzhalter ersetzen oder die Einheitlichkeit deiner Codebasis verbessern möchtest - das Suchen und Ersetzen von Text kann eine zeitaufwändige Aufgabe sein. In diesem Blogpost zeigen wir dir, wie du diese Aufgabe effizient und fehlerfrei mit Ruby bewältigen kannst.

## Wie

Die Suche und Ersetzung von Text in Ruby kann mit der `gsub`-Methode durchgeführt werden. Diese Methode erwartet zwei Argumente: einen Suchstring und einen Ersetzungsstring. Sie kann auf Strings, Arrays und Hashes angewendet werden.

```Ruby
beispiel_string = "Hallo Welt!"
puts beispiel_string.gsub("Welt", "Ruby")
```

Dieser Code gibt "Hallo Ruby!" aus, da der Suchstring "Welt" durch den Ersetzungsstring "Ruby" ersetzt wurde.

Wenn du mehrere Suchbegriffe gleichzeitig ersetzen möchtest, kannst du ein Hash als Argument übergeben:

```Ruby
beispiel_string = "Ruby ist eine tolle Programmiersprache"
puts beispiel_string.gsub({"Ruby" => "Python", "tolle" => "großartige"})
```

Dieser Code gibt "Python ist eine großartige Programmiersprache" aus, da sowohl "Ruby" als auch "tolle" durch ihre jeweiligen Ersetzungsstrings ersetzt wurden.

Auch die Verwendung von Regulären Ausdrücken ist möglich. Das folgende Beispiel ersetzt alle Ziffern in einem String durch den Buchstaben "X":

```Ruby
beispiel_string = "123abc"
puts beispiel_string.gsub(/\d/, "X")
```

Dieser Code gibt "XXXabc" aus. Dabei ersetzt `\d` den Suchstring "123" durch "X".

## Deep Dive

Neben der `gsub`-Methode gibt es in Ruby auch die `sub`-Methode, die nur die erste Übereinstimmung ersetzt, anstatt alle. Außerdem gibt es noch die `replace`-Methode, die den String direkt ändert, während `gsub` und `sub` neue Strings zurückgeben.

Ein weiteres wichtiges Konzept bei der Suche und Ersetzung von Text ist die Verwendung von regulären Ausdrücken. Diese ermöglichen es, komplexere Suchmuster anzugeben, anstatt nur einfache Strings zu suchen. Es gibt zahlreiche Tutorials und Guides im Internet, die sich mit regulären Ausdrücken und ihrer Verwendung in Ruby beschäftigen.

## Siehe Auch

- [Ruby String Dokumentation](https://ruby-doc.org/core-3.0.0/String.html)
- [Ruby regex Tutorial](https://www.rubyguides.com/2015/06/ruby-regex/)
- [Reguläre Ausdrücke mit Ruby](https://www.regular-expressions.info/ruby.html)