---
title:                "Ruby: Verknüpfung von Zeichenketten"
simple_title:         "Verknüpfung von Zeichenketten"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/ruby/concatenating-strings.md"
---

{{< edit_this_page >}}

## Warum 

Das Verketten von Strings ist ein wesentlicher Bestandteil der Ruby-Programmierung. Durch das Verbinden von Strings können wir komplexe Texte und Variablen miteinander kombinieren und so dynamische Inhalte generieren. Es ist ein nützliches Werkzeug, das uns in vielen verschiedenen Szenarien helfen kann.

## Wie geht's?

Um Strings in Ruby zu verketten, verwenden wir den `+` Operator. Dieser verbindet zwei Strings und gibt einen neuen String zurück, der die beiden ursprünglichen Strings enthält. Hier ist ein Beispielcode:

```Ruby
first_name = "Lena"
last_name = "Müller"

full_name = first_name + " " + last_name

puts full_name
```

Output: Lena Müller

In diesem Beispiel haben wir den Vornamen und Nachnamen von Lena verknüpft, um den vollständigen Namen "Lena Müller" zu erhalten. Wir können auch mehrere Strings miteinander verketten, indem wir mehrere `+` Operatoren verwenden. Zum Beispiel:

```Ruby
first_name = "Max"
last_name = "Schmidt"
age = 28

output = first_name + " " + last_name + " is " + age.to_s + " years old."

puts output
```

Output: Max Schmidt is 28 years old.

Es ist wichtig zu beachten, dass beim Verketten von Strings Leerzeichen oder andere Trennzeichen verwendet werden müssen, um ein sauberes Ergebnis zu erhalten. In der letzten Zeile haben wir `to_s` verwendet, um die Zahl "28" in einen String zu konvertieren, da der `+` Operator nur Strings zusammenfügen kann. 

## Tiefer Einblick

Ruby bietet auch die Methode `concat`, um Strings zu verketten. Diese Methode ist ähnlich wie der `+` Operator, hat aber einige Unterschiede. Sie kann zum Beispiel mehrere Argumente entgegennehmen, um Strings zu verketten. Außerdem kann sie auch numerische Werte akzeptieren und diese automatisch in Strings konvertieren. Allerdings ist der `+` Operator in der Regel schneller und effizienter als die `concat` Methode.

## Siehe auch

- [Ruby String Dokumentation](https://ruby-doc.org/core-2.6/String.html)
- [Video-Tutorial: Verketten von Strings in Ruby](https://www.youtube.com/watch?v=fzcq2Orozgw)
- [Tutorial: Ruby-Grundlagen für Anfänger](https://www.guru99.com/ruby-programming-tutorial.html)