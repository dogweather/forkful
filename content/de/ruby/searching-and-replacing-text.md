---
title:    "Ruby: Suchen und Ersetzen von Text"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

"## Warum

Das Suchen und Ersetzen von Text ist eine häufige Aufgabe beim Programmieren. Es ermöglicht die schnelle und präzise Änderung von Text in großen Mengen. In diesem Artikel werden wir uns ansehen, wie man diese Aufgabe in Ruby lösen kann.

## Wie Geht Das?

Das Suchen und Ersetzen von Text in Ruby ist relativ einfach und vielseitig. Um Loszulegen, erstellen wir eine Variable mit unserem Text und setzen die Methode `gsub` ein, die das Ersetzen ermöglicht.

```Ruby
text = "Hallo Welt! Willkommen zu meinem Programmierblog!"
puts text.gsub("Welt", "Programmierwelt")
```
Output: Hallo Programmierwelt! Willkommen zu meinem Programmierblog!

In diesem Beispiel wird das Wort "Welt" durch "Programmierwelt" ersetzt und der geänderte Text wird ausgegeben. Wir können auch reguläre Ausdrücke nutzen, um noch komplexere Such- und Ersetzungsaufgaben durchzuführen.

```Ruby
text = "Das Ergebnis von 2+2 ist 4."
puts text.gsub(/\d+/, "vier")
```
Output: Das Ergebnis von zwei+2 ist vier.

In diesem Beispiel wird jede Zahl im Text durch das Wort "vier" ersetzt, was uns eine sinnvolle Änderung des Satzes ermöglicht.

## Tiefer Einblick

Die Methode `gsub` steht für "global substitution" und ermöglicht die globale Suche und Ersetzung von Text in einem String. Sie akzeptiert zwei Argumente: das Suchmuster und das Ersetzungsmuster. Das Suchmuster kann ein String oder ein regulärer Ausdruck sein und das Ersetzungsmuster kann ebenfalls ein String oder ein Lambda-Ausdruck sein.

Zusätzlich zur Methode `gsub` gibt es auch `sub`, was für "substitution" steht. Im Gegensatz zu `gsub` wird hier nur die erste Übereinstimmung ersetzt.

Weitere Optionen zur Suche und Ersetzung von Text in Ruby bieten die Methoden `gsub!` und `sub!`, die den String direkt verändern, anstatt eine neue Version zurückzugeben.

## Siehe Auch

- [Ruby Dokumentation zu `gsub`](https://ruby-doc.org/core-2.6/String.html#method-i-gsub)
- [Artikel über reguläre Ausdrücke in Ruby](https://www.rubyguides.com/2015/06/ruby-regex/)
- [Übersicht über String Manipulationsmethoden in Ruby](https://www.rubyguides.com/2018/10/string-methods/)