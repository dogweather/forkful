---
title:                "String in Kleinbuchstaben umwandeln"
html_title:           "Ruby: String in Kleinbuchstaben umwandeln"
simple_title:         "String in Kleinbuchstaben umwandeln"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/ruby/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Was ist das und Warum?

Das Konvertieren einer Zeichenkette in Kleinbuchstaben bedeutet, alle Großbuchstaben in einer Zeichenkette in entsprechende Kleinbuchstaben umzuwandeln. Programmierer tun dies, um einheitliche und konsistente Daten zu erhalten, insbesondere bei der Verarbeitung von Eingaben von Benutzern oder Daten aus externen Quellen.

## Wie geht's:

```Ruby
"Hallo WELT".downcase
# => "hallo welt"
```

```Ruby
"123 ABC".downcase
# => "123 abc"
```

### Ausgabe:

```
hallo welt
123 abc
```

## Tief eintauchen:

### Historischer Kontext:
Die Idee, Großbuchstaben in Kleinbuchstaben umzuwandeln, geht auf die frühen Tage des Computerprogrammierens zurück, als die Zeichenkodierung ASCII (American Standard Code for Information Interchange) verwendet wurde. In ASCII ist jeder Großbuchstabe durch ein entsprechendes kleines Pendant dargestellt, was zu der Konvention führte, alle Zeichen in Kleinbuchstaben umzuwandeln, um die Lesbarkeit zu verbessern.

### Alternativen:
Neben der Verwendung der `downcase` Methode gibt es auch andere Möglichkeiten, eine Zeichenkette in Kleinbuchstaben zu konvertieren. Zum Beispiel kann die Methode `capitalize` verwendet werden, um nur den ersten Buchstaben einer Zeichenkette in einen Großbuchstaben umzuwandeln.

### Implementierungsdetails:
In Ruby wird die `downcase` Methode durch die `String` Klasse bereitgestellt, die in der Standardbibliothek enthalten ist. Sie wird von allen String-Objekten aufgerufen und liefert eine neue Zeichenkette mit allen Buchstaben in Kleinbuchstaben zurück.

## Siehe auch:

- [Ruby String-Dokumentation](https://ruby-doc.org/core-2.7.0/String.html#method-i-downcase)
- [ASCII Character Encoding](https://www.ascii-code.com/)