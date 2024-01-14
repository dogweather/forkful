---
title:                "Ruby: Eine Zeichenfolge in Kleinbuchstaben umwandeln."
programming_language: "Ruby"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/ruby/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# Warum

Das Konvertieren von einem String in Kleinbuchstaben kann hilfreich sein, um beispielsweise Benutzereingaben in einer Datenbank zu vereinheitlichen oder Textfilterungen durchzuführen.

# Wie

Die Ruby-Methode `downcase` kann verwendet werden, um einen String in Kleinbuchstaben umzuwandeln. Beispiel:

```Ruby
string = "HELLO WORLD"
puts string.downcase
```
Ausgabe: `hello world`

Für die Verwendung mit deutschen Umlauten kann auch die Methode `unicode_normalize` genutzt werden. Beispiel:

```Ruby
string = "Äpfel und Birnen"
puts string.downcase.unicode_normalize(:nfkd) # the :nfkd option decomposes complex characters into a single equivalent
```
Ausgabe: `äpfel und birnen`

# Tiefgehende Erkundung

Bei der Konvertierung von Strings sollte beachtet werden, dass einige Sonderzeichen und Satzzeichen nicht in Kleinbuchstaben umgewandelt werden. Beispielsweise wird `'Äpfel'.downcase` kein Ergebnis liefern, da der Buchstabe "Ä" nicht zu "ä" konvertiert werden kann.

Um dieses Problem zu umgehen, kann die Ruby-Methode `unicode_normalize` in Kombination mit dem Parameter `:nfkd` verwendet werden, wie im obigen Beispiel gezeigt.

Eine weitere Sache, die beachtet werden muss, ist die Verwendung von Sprachen mit mehr als einem Alphabet, wie beispielsweise Chinesisch oder Koreanisch. In diesen Fällen kann die Konvertierung von Strings in Kleinbuchstaben eine komplizierte Angelegenheit sein und erfordert möglicherweise spezielle Methoden, die sich auf die jeweiligen Schriften beziehen.

# Siehe auch

- [Ruby-Dokumentation zu String-Klassenmethoden](https://ruby-doc.org/core-3.0.0/String.html#method-c-new)
- [Ruby-Dokumentation zu String-Methoden](https://ruby-doc.org/core-3.0.0/String.html#method-i-downcase)
- [Ruby-Forum-Thread zu Konvertierung von Strings mit Umlauten](https://www.ruby-forum.com/t/string-doesnt-change-after-downcase/182596)