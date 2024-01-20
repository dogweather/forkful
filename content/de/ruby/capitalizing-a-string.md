---
title:                "Einen String großschreiben"
html_title:           "Ruby: Einen String großschreiben"
simple_title:         "Einen String großschreiben"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/ruby/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Großschreiben eines Strings (also das Umwandeln des ersten Buchstabens einer Zeichenkette in einen Großbuchstaben) ist eine übliche Praxis in der Programmierung. Es erleichtert die Lesbarkeit und kann auch bei der Sortierung von Daten helfen.

## So geht's:

In Ruby benutzen wir die Methode `.capitalize` um einen String zu großzuschreiben. Hier ein paar Beispiele:
 
```Ruby
name = "ruby programmer"
puts name.capitalize
```
Das wird "Ruby programmer" auf dem Bildschirm ausgeben.

```Ruby
greeting = "hallo, welt!"
puts greeting.capitalize
```
Dieser Code wird "Hallo, welt!" ausgeben.

## Tiefer Einblick:

Das Konzept der Großschreibung eines Strings hat eine lange Geschichte und wir finden es in den meisten Programmiersprachen. Alternativ kann man auch die Methode `upcase` verwenden, um alle Buchstaben eines Strings großzuschreiben. Die Methode `.capitalize` in Ruby wurde jedoch so implementiert, dass sie nur den ersten Buchstaben eines Strings großschreibt.

## Siehe auch:

- Ruby-Dokumentation über Stringfunktionen: [https://ruby-doc.org/core/String.html](https://ruby-doc.org/core/String.html)
- Interessanter Diskussionsthread zu `.capitalize` vs `.titleize`: [https://stackoverflow.com/questions/1352019/ruby-capitalize-every-word-first-letter](https://stackoverflow.com/questions/1352019/ruby-capitalize-every-word-first-letter)
- Online-Ruby-Kurs für Anfänger: [https://www.codecademy.com/learn/learn-ruby](https://www.codecademy.com/learn/learn-ruby)