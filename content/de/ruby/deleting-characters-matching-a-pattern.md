---
title:                "Ruby: Löschen von Zeichen, die einem Muster entsprechen"
simple_title:         "Löschen von Zeichen, die einem Muster entsprechen"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/ruby/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Warum

Das Löschen von Zeichen, die einem bestimmten Muster entsprechen, kann in der Programmierung nützlich sein, um ungewünschte Zeichen aus einem String zu entfernen oder Daten zu bereinigen.

## Wie geht das?

Um Zeichen in einem String zu löschen, die einem bestimmten Muster entsprechen, können wir die `.gsub()` Methode in Ruby verwenden. Wir übergeben ihr das Muster, das wir suchen, und ersetzen es durch einen leeren String. Hier ist ein Beispiel:

```Ruby
string = "Hallo Welt!"
neuer_string = string.gsub(/[A-Z]/, "")
puts neuer_string # => a o elt!
```

Wie wir sehen können, wurden alle Großbuchstaben im String gelöscht, da wir dieses Muster in unserer `.gsub()` Methode verwendet haben. Hier ist noch ein Beispiel, um alle Zahlen aus einem String zu entfernen:

```Ruby
string = "Heute ist der 15. Juli"
neuer_string = string.gsub(/\d/, "")
puts neuer_string # => Heute ist der  . Juli
```

In diesem Beispiel haben wir das Muster `\d` verwendet, das für eine beliebige Zahl steht. Dadurch werden alle Zahlen in unserem String gelöscht.

## Tiefer tauchen

Wir haben bisher nur einfache Beispiele betrachtet, aber `.gsub()` kann noch viel mehr! Sie kann auch mit sogenannten regulären Ausdrücken (`Regexp`) arbeiten, die es uns ermöglichen, komplexere Muster anzugeben. Zum Beispiel könnten wir alle Wörter in einem String löschen, die mit einem Vokal beginnen:

```Ruby
string = "Ich mag Äpfel und Orangen"
neuer_string = string.gsub(/\b[aeiou]\w+/, "")
puts neuer_string # => Ich g gestern und
```

In diesem Beispiel haben wir das Muster `\b[aeiou]\w+` verwendet, das für ein Wort steht, das mit einem Vokal beginnt und danach beliebige Zeichen enthält. Durch das Voranstellen des Zeichens `\b` stellen wir sicher, dass wir nur ganze Wörter und nicht Teilwörter löschen.

## Siehe auch

- https://ruby-doc.org/core-2.6.3/String.html#method-i-gsub
- https://www.rubyguides.com/2019/02/ruby-gsub-method/
- https://www.geeksforgeeks.org/ruby-strings-gsub-function/