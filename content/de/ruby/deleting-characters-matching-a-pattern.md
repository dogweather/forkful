---
title:    "Ruby: Musterübereinstimmende Zeichen löschen"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/ruby/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Warum

Das Löschen von Zeichen, die einem bestimmten Muster entsprechen, kann in der Ruby-Programmierung sehr nützlich sein, um bestimmte Textabschnitte oder Zeichenketten zu entfernen. Dies kann zum Beispiel bei der Datenbereinigung oder bei der Manipulation von Textdateien hilfreich sein.

## How To

Um in Ruby Zeichen zu löschen, die einer bestimmten Bedingung entsprechen, verwenden wir die "gsub" Methode zusammen mit einem regulären Ausdruck. Dieser reguläre Ausdruck definiert das Muster, nach dem wir suchen möchten. In folgendem Beispiel entfernen wir alle Leerzeichen aus einer Zeichenkette:

```Ruby
text = "Dies ist ein Beispieltext"
text.gsub(/\s+/, "")
#Output: "DiesisteinBeispieltext"
```

Der reguläre Ausdruck `/\s+/` sucht nach einem oder mehreren aufeinanderfolgenden Leerzeichen und ersetzt diese durch einen leeren String. Wir können auch andere Zeichen oder Zeichenkombinationen löschen, indem wir den regulären Ausdruck entsprechend ändern.

## Deep Dive

Für diejenigen, die tiefer in die Welt der regulären Ausdrücke eintauchen möchten, gibt es zahlreiche Möglichkeiten, Zeichen zu löschen, die einem bestimmten Muster entsprechen. Zum Beispiel können wir den regulären Ausdruck mit Zeichenklassen wie `\d` für Zahlen oder `\w` für alphanumerische Zeichen kombinieren, um spezifischere Muster anzugeben. Wir können auch Optionen wie "global" oder "ignore case" verwenden, um das Verhalten des regulären Ausdrucks anzupassen.

Ein hilfreiches Werkzeug zum Experimentieren und Testen von regulären Ausdrücken ist die Online-Plattform Rubular (https://rubular.com/). Hier können wir unsere regulären Ausdrücke eingeben und sofort sehen, welche Zeichen damit übereinstimmen und welche gelöscht werden würden.

## Siehe auch

- Ruby Dokumentation zu regulären Ausdrücken: https://ruby-doc.org/core-2.7.1/Regexp.html
- Einführung in reguläre Ausdrücke: https://www.rubyguides.com/2015/06/ruby-regex/
- Weitere Anwendungsmöglichkeiten von regulären Ausdrücken in Ruby: https://medium.com/@rubylearning/ruby-regexes-739ce7d7f444