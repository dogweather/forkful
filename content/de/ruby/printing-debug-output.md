---
title:                "Ruby: Ausgabe von Debug-Informationen"
simple_title:         "Ausgabe von Debug-Informationen"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/ruby/printing-debug-output.md"
---

{{< edit_this_page >}}

# Warum

Das Drucken von Debug-Ausgaben ist ein wesentlicher Bestandteil des Programmierens, da es dabei hilft, Fehler in unserem Code zu finden und zu beheben. Debugging kann oft zeitaufwendig sein, aber mit dem Drucken von Debug-Ausgaben können wir den Prozess beschleunigen und unsere Codequalität verbessern.

# Wie man das macht

Um Debug-Ausgaben in Ruby zu drucken, können wir die `puts` Funktion verwenden. Hier ist ein einfaches Beispiel:

```Ruby
def add(a, b)
  sum = a + b
  puts "Die Summe von #{a} und #{b} ist #{sum}."
  return sum
end

add(3, 5)
```

Die Ausgabe dieses Codes wird folgendes sein:

```Ruby
Die Summe von 3 und 5 ist 8.
```

Wir können auch Variablen oder einzelne Werte drucken, um den Wert zu überprüfen. Hier ist ein weiteres Beispiel:

```Ruby
x = 10
puts x
```

Die Ausgabe dieses Codes wird `10` sein.

# Tieferer Einblick

Neben der Verwendung von `puts` gibt es auch andere Methoden, um Debug-Ausgaben in Ruby zu drucken. Eine weitere Möglichkeit ist die Verwendung von `p`, die die Ausgabe formatiert und auch den Datentyp anzeigt. Zum Beispiel:

```Ruby
x = 10
p x
```

Die Ausgabe dieses Codes wird `10` und `Integer` sein.

Es gibt auch Debugger-Tools wie `byebug` oder `pry`, die es uns ermöglichen, Schritt für Schritt durch unseren Code zu gehen und die Werte der Variablen zu überprüfen. Diese Tools sind besonders hilfreich bei der Fehlerbehebung von komplexen Programmen.

# Siehe auch

- [Ruby-Dokumentation zu `puts`](https://ruby-doc.org/core-2.7.1/Kernel.html#method-i-puts)
- [Artikel über Debugging in Ruby](https://medium.com/@byrningtyger/ruby-debugging-in-practice-2f9dbdef244d)