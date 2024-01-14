---
title:                "Ruby: Die Länge eines Strings finden."
simple_title:         "Die Länge eines Strings finden."
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/ruby/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Warum

Strings sind ein grundlegender Bestandteil der meisten Programmiersprachen, einschließlich Ruby. Es ist wichtig zu wissen, wie man die Länge eines Strings bestimmen kann, um effektiv mit ihnen umgehen zu können.

## Wie geht's

Um die Länge eines Strings zu finden, können wir die `.length` Methode in Ruby verwenden. Hier ist ein einfaches Beispiel:

```Ruby
string = "Hallo Welt"
puts string.length
```

Dies würde die Ausgabe `10` zurückgeben, da der String "Hallo Welt" 10 Zeichen lang ist.

Wir können auch die `.size` Methode verwenden, um die Länge eines Strings zu ermitteln. Diese Methode gibt jedoch die Anzahl der Bytes zurück, die der String belegt. In den meisten Fällen sollte dies kein Problem darstellen, aber es ist wichtig, dies im Hinterkopf zu behalten, wenn du mit Dateien oder speziellen Zeichen arbeitest.

```Ruby
string = "Hola Mundo"
puts string.size
```

Dies würde ebenfalls `10` zurückgeben, da sowohl `.length` als auch `.size` die Anzahl der Zeichen im String zurückgeben.

## Tiefer Einblick

Um besser zu verstehen, wie Strings in Ruby funktionieren, können wir uns die interne Implementierung der `.length` Methode ansehen. Diese Methode verwendet das `String#capi_length` Rückrufobjekt, das die Länge des Strings mithilfe von C-Code bestimmt. Dies macht die Ausführung effizienter, indem es die Verarbeitungsgeschwindigkeit erhöht.

Die `.length` Methode kann auch auf Arrays angewendet werden, um ihre Länge zu bestimmen. Dies liegt daran, dass Strings in Ruby im Grunde genommen ein spezieller Typ von Array sind.

## Siehe auch

- [Ruby Strings Dokumentation](https://ruby-doc.org/core-2.6.3/String.html)
- [Codecademy Ruby Kurs](https://www.codecademy.com/learn/learn-ruby)
- [Ruby Spezifikationen](https://github.com/ruby/spec)