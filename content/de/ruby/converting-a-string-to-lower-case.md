---
title:                "Eine Zeichenfolge in Kleinbuchstaben umwandeln"
html_title:           "Ruby: Eine Zeichenfolge in Kleinbuchstaben umwandeln"
simple_title:         "Eine Zeichenfolge in Kleinbuchstaben umwandeln"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/ruby/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Warum

Eines der häufigsten Probleme beim Programmieren ist die Konsistenz der Daten. Oftmals haben wir unterschiedliche Schreibweisen von Wörtern, sei es durch Tippfehler oder durch externe Einflüsse. Um dieses Problem zu lösen und die Daten zu vereinheitlichen, ist es wichtig, eine Funktion zu haben, die Strings in Kleinbuchstaben umwandeln kann.

## Wie es geht

Um einen String in Kleinbuchstaben zu konvertieren, können wir in Ruby die `downcase` Methode verwenden. Hier ist ein Beispielcode:

```Ruby
name = "MaRiA"
puts name.downcase
# Ausgabe: maria
```

Die `downcase` Methode gibt eine neue Kopie des String-Objekts zurück, in dem alle Buchstaben in Kleinbuchstaben umgewandelt sind. Dies ermöglicht es uns, den ursprünglichen String unverändert zu lassen und trotzdem die gewünschte Formatierung zu erhalten.

## Tiefere Einblicke

Neben der `downcase` Methode gibt es auch noch die `downcase!` Methode, die den ursprünglichen String direkt ändert, anstatt eine Kopie zurückzugeben. Dies kann nützlich sein, wenn wir den String dauerhaft ändern möchten.

Bei der Umwandlung in Kleinbuchstaben ist es wichtig zu beachten, dass diese Methode nur die Buchstaben im ASCII-Zeichensatz umwandelt. Andere Zeichen, wie zum Beispiel Umlaute, werden nicht automatisch in Kleinbuchstaben umgewandelt. In solchen Fällen müssen wir möglicherweise auf andere Methoden wie `unicode_normalize` zurückgreifen, um alle Zeichen zu berücksichtigen.

## Siehe auch

1. [Ruby String Dokumentation](https://ruby-doc.org/core-2.7.1/String.html)
2. [ASCII-Tabelle](https://de.wikipedia.org/wiki/American_Standard_Code_for_Information_Interchange)
3. [Ruby `unicode_normalize` Methode](https://apidock.com/ruby/String/unicode_normalize)