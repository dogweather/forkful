---
title:                "Großschreibung eines Strings"
html_title:           "Ruby: Großschreibung eines Strings"
simple_title:         "Großschreibung eines Strings"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/ruby/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# Was und Warum?
Die Großschreibung (auch bekannt als Capitalization) ist ein gängiger Begriff in der Programmierung, der die Änderung des ersten Buchstabens eines Strings in einen Großbuchstaben bezeichnet. Dies geschieht aus verschiedenen Gründen, beispielsweise um die Lesbarkeit des Codes zu verbessern oder um bestimmte Konvertierungen durchzuführen.

# Wie geht das?
Die Großschreibung eines Strings in Ruby ist einfach und erfordert nur wenige Zeilen Code. Hier ist ein Beispiel:

```Ruby
str = "hallo welt"
puts str.capitalize
```

Die Ausgabe dieses Codes ist "Hallo welt". Der Befehl `capitalize` lässt Ruby den ersten Buchstaben in einen Großbuchstaben konvertieren.

# Tiefergehende Informationen
Historisch gesehen gibt es verschiedene Möglichkeiten, um Strings in ein bestimmtes Format zu bringen, wie z.B. die Verwendung von ASCII-Codes oder regulären Ausdrücken. In Ruby gibt es jedoch die `capitalize` Methode, die speziell für die Großschreibung von Strings entwickelt wurde.

Alternativ gibt es auch die Methode `upcase`, die alle Buchstaben in einen String in Großbuchstaben konvertiert. Beachten Sie jedoch, dass dies nicht dasselbe wie `capitalize` ist, da `upcase` alle Buchstaben in Großbuchstaben ändert, während `capitalize` nur den ersten Buchstaben verändert.

Die `capitalize` Methode kann auch auf Wörter angewendet werden, die mehr als einen Buchstaben enthalten, ohne den Rest des Worts zu ändern. Zum Beispiel würde `Hello World`.capitalize zu `Hello world` werden, während `Hello World`.upcase immer noch `HELLO WORLD` sein würde.

# Weitere Informationen
Für weitere Informationen zu Strings und ihrer Formatierung in Ruby können Sie die offizielle Dokumentation von Ruby oder die zahlreichen Online-Tutorials konsultieren.

## Siehe auch
- Offizielle Ruby-Dokumentation: https://ruby-doc.org/core-2.7.0/String.html
- Ruby-Tutorial: https://www.ruby-lang.org/de/documentation/quickstart/