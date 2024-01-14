---
title:    "Ruby: Auslesen von Teilzeichenfolgen"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Warum?
Eine der nützlichsten Techniken im Ruby-Programmieren ist das Extrahieren von Substrings. Diese Methode erlaubt es Ihnen, Teile von Strings basierend auf bestimmten Bedingungen auszuwählen und zu manipulieren. Es ist eine unverzichtbare Fähigkeit für die Bearbeitung von Texten und das Durchsuchen von Daten.

## Wie gehts?

Um einen Substring in Ruby zu extrahieren, verwenden Sie die `slice` oder `[]` Methode mit dem Index des Anfangs- und Endzeichen. Zum Beispiel, wenn wir den Substring "Ruby" aus dem String "Ich liebe Ruby-Programmieren" extrahieren wollen, würden wir Folgendes tun:
 
```Ruby
text = "Ich liebe Ruby-Programmieren"
substring = text.slice(8, 4)  # oder text[8, 4]
puts substring  # Ausgabe: "Ruby"
```

Sie können auch `slice` oder `[]` mit Range-Objekten verwenden, um den Substring anhand von Zeichenpositionen auszuwählen. Zum Beispiel, um die ersten drei Zeichen aus dem String "Ruby ist einfach großartig" zu extrahieren, müssten wir Folgendes tun:

```Ruby
text = "Ruby ist einfach großartig"
substring = text.slice(0..2)  # oder text[0..2]
puts substring  # Ausgabe: "Rub"
```

Eine weitere Möglichkeit, Substrings in Ruby zu extrahieren, ist die Verwendung von regulären Ausdrücken. Mit Hilfe von RegEx können Sie Substrings basierend auf bestimmten Mustern auswählen und manipulieren. Zum Beispiel, um alle Vokale aus dem String "Hallo Welt" zu extrahieren, könnten wir Folgendes tun:

```Ruby
text = "Hallo Welt"
vowels = text.scan(/[aeiou]/)
puts vowels.join("")  # Ausgabe: "aoe"
```

## Tiefer Tauchen

Die `slice` und `[]` Methoden nehmen auch negative Indizes an, was es Ihnen ermöglicht, von hinten zu zählen. Zum Beispiel, um den letzten Buchstaben aus einem String zu extrahieren, könnten wir Folgendes tun:

```Ruby
text = "Hallo"
last_letter = text.slice(-1)  # oder text[-1]
puts last_letter  # Ausgabe: "o"
```

Sie können auch die `slice!` oder `slice!` Methoden verwenden, um den ausgewählten Substring direkt aus dem ursprünglichen String zu entfernen. Zum Beispiel:

```Ruby
text = "Hallo Welt"
text.slice!(6..8)  # text wird jetzt "Hallo"
puts text  # Ausgabe: "Hallo"
```

## Siehe auch

- [Ruby String Dokumentation] (https://ruby-doc.org/core-2.7.1/String.html)
- [Reguläre Ausdrücke in Ruby] (https://www.rubyguides.com/2015/06/ruby-regex/)
- [Ruby Substrings-Beispiele] (https://www.tutorialspoint.com/ruby/ruby_strings.htm)