---
title:                "Verwendung von regulären Ausdrücken"
html_title:           "Ruby: Verwendung von regulären Ausdrücken"
simple_title:         "Verwendung von regulären Ausdrücken"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/ruby/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Was & Warum?
Reguläre Ausdrücke sind Muster oder Musterfolgen, die verwendet werden, um Text zu suchen und zu manipulieren. Programme nutzen reguläre Ausdrücke, um eine effiziente und präzise Möglichkeit zu haben, bestimmte Zeichenkombinationen im Text zu finden und zu bearbeiten.

## Wie geht das:
Um reguläre Ausdrücke in Ruby zu verwenden, benutze die `=~` Operator und zwei forward Slashes (/ /) um den Ausdruck anzugeben, den du suchen möchtest. Hier ist ein einfaches Beispiel:

```Ruby
str = “Hallo, ich bin ein Ruby Programmierer!”
if str =~ /Hallo/
  puts “Ich wurde gefunden!”
else
  puts “Leider nicht gefunden…”
end
```

Das obere Beispiel sucht nach dem Wort "Hallo" in der Variable `str`. Wenn es gefunden wird, wird die entsprechende Nachricht ausgegeben.

## Tiefer in die Materie:
Reguläre Ausdrücke wurden in den 50er Jahren entwickelt und fanden in den 60er Jahren ihren Weg in die Programmierung. Es gibt auch andere Optionen, um Text zu suchen und zu manipulieren, wie z.B. die Verwendung von String-Operationen in Ruby. Reguläre Ausdrücke bieten jedoch eine präzisere und effizientere Möglichkeit, komplexe Muster im Text zu finden und zu bearbeiten.

Um reguläre Ausdrücke in Ruby zu implementieren, verwendet Ruby die Oniguruma-Bibliothek. Diese Bibliothek unterstützt alle regulären Ausdrucksoperatoren und bietet auch einige zusätzliche Funktionen, wie z.B. die Möglichkeit, bedingte Ausdrücke zu verwenden.

## Sieh dir auch an:
- [Eine praktische Anleitung zu regulären Ausdrücken in Ruby](https://www.rubyguides.com/2015/06/ruby-regular-expressions/)
- [Die Oniguruma-Bibliothek auf Ruby-Doc.org](https://ruby-doc.org/core-2.7.0/Oniguruma.html)
- [Reguläre Ausdrücke als Werkzeug für effiziente Textmanipulation](https://medium.com/free-code-camp/an-introduction-to-regular-expressions-regex-in-ruby-5786cf8b900c)