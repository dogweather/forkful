---
title:    "Ruby: Suchen und Ersetzen von Text"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/ruby/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# Warum

Das Suchen und Ersetzen von Text ist eine nützliche Fähigkeit, die jeder Ruby-Programmierer beherrschen sollte. Es ermöglicht das effiziente Bearbeiten von Textdateien und -strings, was besonders bei der Bearbeitung großer Datenmengen hilfreich ist.

# Wie man Text sucht und ersetzt

Zur Durchführung von Such- und Ersetzungsvorgängen in Ruby gibt es verschiedene Methoden, die wir uns genauer anschauen werden.

## String#sub und String#gsub

Die Methoden `sub` und `gsub` können auf Strings angewendet werden, um bestimmte Teilstrings zu suchen und zu ersetzen. Der Unterschied zwischen den beiden besteht darin, dass `sub` nur die erste Übereinstimmung ersetzt, während `gsub` alle Übereinstimmungen ersetzt.

```ruby 
# Beispiel mit String#sub
"My name is John".sub("John", "Jane") # => "My name is Jane"

# Beispiel mit String#gsub
"Hello Ruby, welcome to Ruby".gsub("Ruby", "World") # =>"Hello World, welcome to World"
```

## Regex

Die Verwendung von Regular Expressions ermöglicht es, komplexe Such- und Ersetzungsvorgänge durchzuführen. Um einen Regex in Ruby zu erstellen, verwenden wir den Konstruktor `/.../`. Im folgenden Beispiel wird der Regex `/[aeiou]/` verwendet, um alle Vokale in einem String zu ersetzen.

```ruby
"Hello World".gsub(/[aeiou]/, "*") # => "H*ll* W*rld"
```

# Tiefergehende Informationen

Beim Ersetzen von Text gibt es noch weitere wichtige Aspekte zu beachten, wie z.B. die Verwendung von Modifikatoren oder die Auswirkungen auf die Performance. Für eine ausführliche Erklärung empfehle ich die offizielle Dokumentation oder weitere Tutorials zu diesem Thema.

# Siehe auch

- [Offizielle Dokumentation zu String#sub und String#gsub](https://ruby-doc.org/core-2.6.3/String.html#method-i-gsub)
- [Regex Tutorial für Ruby](https://rubular.com/)
- [Praxisbeispiel für die Verwendung von Regex zum Suchen und Ersetzen in Ruby](https://www.rubyguides.com/2019/07/ruby-regex/)

Vielen Dank fürs Lesen! Falls du weitere Fragen hast oder Feedback zu diesem Beitrag hast, zögere nicht, einen Kommentar zu hinterlassen. Happy coding!