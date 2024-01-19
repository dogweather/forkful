---
title:                "Teilzeichenketten extrahieren"
html_title:           "PowerShell: Teilzeichenketten extrahieren"
simple_title:         "Teilzeichenketten extrahieren"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/ruby/extracting-substrings.md"
---

{{< edit_this_page >}}

## Was & Warum?

Substring Extraktion ist der Vorgang, bei dem ein Teilstring aus einem größeren String herausgeholt wird. Das ist nützlich, um spezifische Daten oder Muster innerhalb eines Texts zu isolieren.

## Wie macht man das?

```Ruby
text = "Hallo, Welt!"
substring = text[7,5]
puts substring
```
Output:
```Ruby
Welt!
```

Eine andere Methode ist `.slice()`. 

```Ruby
text = "Ich liebe Ruby!"
substring = text.slice(7,4)
puts substring
```
Output:
```Ruby
Ruby
```
## Vertiefung

Die Substring-Extraktion gibt es schon seit den frühen Tagen der Programmiersprachen. In Ruby gibt es mehrere Methoden, um Substrings zu extrahieren, einschließlich `slice()`, `[]` und `slice!()`. Es sei zu beachten, dass `slice!()` das Original verändert, indem es den extrahierten Substring entfernt.

Alternative Methoden sind die Verwendung von regulären Ausdrücken, um spezifischere oder komplexere Muster zu extrahieren.

Der Ruby Interpreter extrahiert Substrings, indem er den Index jedes Zeichens in einem String als Ausgangspunkt verwendet. Er zählt dann die Anzahl der folgenden Zeichen, die sich aus der angegebenen Länge ergeben.

## Siehe auch

Ruby-Dokumentation zu `slice()`: https://ruby-doc.org/core-2.7.0/String.html#method-i-slice

Ruby-Dokumentation zu `[]`: https://ruby-doc.org/core-2.7.0/String.html#method-i-5B-5D

Einführung zu regulären Ausdrücken in Ruby: https://ruby-doc.org/core-2.5.1/Regexp.html