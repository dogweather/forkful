---
title:                "Eine Zeichenkette großschreiben"
html_title:           "Ruby: Eine Zeichenkette großschreiben"
simple_title:         "Eine Zeichenkette großschreiben"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/ruby/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Warum

Jeder hat wahrscheinlich schon mal einen Satz oder einen Namen in einer E-Mail oder Textnachricht in Großbuchstaben geschrieben, um eine Betonung auszudrücken oder zu zeigen, dass man wütend ist. Aber wusstest du, dass es auch in der Programmierung wichtig sein kann, Strings (Zeichenketten) in Großbuchstaben umzuwandeln? Mit Ruby, einer modernen und benutzerfreundlichen Programmiersprache, ist das ganz einfach. In diesem Artikel werde ich dir zeigen, wie du Strings in Ruby in Großbuchstaben umwandeln kannst und warum das wichtig sein kann.

## Wie geht's

Das Umwandeln von Strings in Großbuchstaben ist in Ruby dank einer eingebauten Methode namens `upcase` unkompliziert. Hier ist ein Beispiel:

```Ruby
string = "hallo welt"
puts string.upcase
# Ausgabe: HALLO WELT
```

Wie du sehen kannst, gibt die Methode `upcase` den String in Großbuchstaben aus. Diese Methode funktioniert auch mit Umlauten und Sonderzeichen:

```Ruby
string = "Entschuldigung, ich spreche kein Deutsch."
puts string.upcase
# Ausgabe: ENTSCHULDIGUNG, ICH SPRECHE KEIN DEUTSCH.
```

Du kannst die Methode auch auf Variablen anwenden, die vorherige Methoden auf Strings ausgeführt haben:

```Ruby
string = "Hallo welt"
puts string.reverse.upcase
# Ausgabe: TLEW OLLAH
```
## Tief tauchen

Das Umwandeln von Strings in Großbuchstaben ist besonders nützlich, wenn du mit Benutzereingaben arbeitest. Nehmen wir an, du baust eine Benutzerregistrierungsseite für deine Website und möchtest sicherstellen, dass alle Benutzernamen in Großbuchstaben gespeichert werden, um spätere Verwirrungen oder Fehler zu vermeiden. Oder du möchtest eine Suchfunktion für deine Webseite erstellen und möchtest sicherstellen, dass alle eingegebenen Suchbegriffe unabhängig von der Groß- und Kleinschreibung funktionieren.

Eine weitere interessante Anwendung von `upcase` ist die Verwendung in Kombination mit der `slice`-Methode, um bestimmte Buchstaben in einem String zu ersetzen oder zu entfernen. Hier ist ein Beispiel:

```Ruby
string = "Hallo Welt"
puts string.slice(0..4).upcase + string.slice(5..-1)
# Ausgabe: HALLOWelt
```

In diesem Beispiel wird die `slice`-Methode verwendet, um aus dem ursprünglichen String nur die ersten fünf Buchstaben (`"Hallo"`) auszuwählen und dann `upcase` angewendet, um sie in Großbuchstaben auszugeben. Die restlichen Buchstaben werden durch den zweiten Teil des `puts`-Statements unverändert ausgegeben.

## Siehe auch

- [Ruby Dokumentation - Strings](https://ruby-doc.org/core-2.7.1/String.html)
- [ruby-doc.org - Methode upcase](https://ruby-doc.org/core-2.7.1/String.html#method-i-upcase)
- [rubyguides.com - Ruby Strings](https://www.rubyguides.com/ruby-string/)