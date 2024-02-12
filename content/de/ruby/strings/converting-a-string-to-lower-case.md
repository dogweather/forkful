---
title:                "Umformung eines Strings in Kleinbuchstaben"
aliases:
- /de/ruby/converting-a-string-to-lower-case/
date:                  2024-01-20T17:39:12.090187-07:00
model:                 gpt-4-1106-preview
simple_title:         "Umformung eines Strings in Kleinbuchstaben"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/ruby/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Umwandeln eines Strings in Kleinbuchstaben bedeutet, jeden Großbuchstaben in seiner kleinen Form darzustellen. Programmierer nutzen das, um die Groß- und Kleinschreibung in der Verarbeitung von Text zu vereinheitlichen, was bei Suchfunktionen oder beim Vergleichen von Zeichenketten hilfreich ist.

## Anleitung:

```ruby
# String in Kleinbuchstaben umwandeln
text = "Heute ist EIN guter Tag zum CODEN!"
klein_text = text.downcase

puts klein_text
```

Ausgabe:

```
heute ist ein guter tag zum coden!
```

## Tiefere Einblicke:

In den Anfängen der Computerprogrammierung waren viele Systeme casesensitiv, was bedeutet, dass Groß- und Kleinschreibung unterschieden wurde. In der heutigen Textverarbeitung dient das Umwandeln in Kleinbuchstaben oft der Normalisierung von Eingaben, um sie leichter vergleichbar zu machen.

Alternativen zum `.downcase`-Methodenaufruf in Ruby gibt es in Form von `.downcase!`, welches den String "in-place" modifiziert, also das ursprüngliche Objekt verändert, statt eine Kopie zurückzugeben.

Implementierungsdetails: Ruby's `.downcase` Methode berücksichtigt auch locale-spezifische Fälle in der Umwandlung, zum Beispiel wird "ß" nicht in "ss" umgewandelt, da es in Kleinbuchstaben bereits korrekt ist. Die Methode funktioniert Unicode-kompatibel, was bedeutet, dass sie mit einer Vielzahl von Schriftsystemen und Sonderzeichen umgehen kann.

## Siehe auch:

- Die Ruby-Dokumentation zur `.downcase` Methode: [Ruby Doc: String#downcase](https://ruby-doc.org/core/String.html#method-i-downcase)
- Ein Vergleich der String-Methoden `.downcase` und `.downcase!`: [Ruby Doc: downcase vs. downcase!](https://ruby-doc.org/core/String.html#method-i-downcase-21)
- Informationen zu Unicode in Ruby: [Ruby Doc: Encoding](https://ruby-doc.org/core/Encoding.html)
