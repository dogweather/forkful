---
title:                "Unterstrings extrahieren"
html_title:           "Ruby: Unterstrings extrahieren"
simple_title:         "Unterstrings extrahieren"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/ruby/extracting-substrings.md"
---

{{< edit_this_page >}}

## Warum

Wenn du schon eine Weile mit Ruby programmiert hast, hast du vielleicht schon mal die Notwendigkeit gehabt, einen Teil eines Strings zu extrahieren. Das Extrahieren von Substrings ist eine nützliche Technik, um Texte zu manipulieren und können dir dabei helfen, komplexe Aufgaben zu lösen.

## Wie geht das

Um einen Substring aus einem String zu extrahieren, kannst du die `[]`-Notation verwenden. Hier ist ein Beispiel, wie du den dritten bis fünften Buchstaben aus dem Wort "Ruby" extrahieren kannst:

```Ruby
"Ruby"[2..4]
```

Das Ergebnis wäre `"by"`, da die Zählung bei Strings in Ruby bei 0 beginnt. Du kannst auch negative Indizes verwenden, um die Zeichen von hinten auszuwählen. Zum Beispiel:

```Ruby
"Ruby"[-3..-1]
```

Dies würde auch in `"by"` resultieren, da `-1` das letzte Element im String ist.

Du kannst auch die `slice`-Methode verwenden, um Substrings zu extrahieren. Hier ist ein Beispiel:

```Ruby
"Ruby".slice(1..3)
```

Das Ergebnis wäre wiederum `"uby"`.

## Tiefentauchen

Du kannst mit Substrings viel mehr machen, als nur Zeichen auszuwählen. Du kannst zum Beispiel nach bestimmten Mustern suchen und sie extrahieren. Dafür kannst du die `scan`-Methode verwenden. Hier ist ein Beispiel, um alle Zahlen aus einem String zu extrahieren:

```Ruby
"Ruby2021istgroßartig".scan(/\d+/)
```

Das Ergebnis wäre eine Array mit einem Element, nämlich `"2021"`.

Du kannst auch mit regulären Ausdrücken `gsub` verwenden, um bestimmte Muster in einem String zu ersetzen. Hier ist ein Beispiel, um alle Leerzeichen in einem String durch ein Minus-Zeichen zu ersetzen:

```Ruby
"Ruby ist toll".gsub(" ", "-")
```

Das Ergebnis wäre `"Ruby-ist-toll"`.

## Siehe auch

- [String-Klasse in Ruby](https://ruby-doc.org/core-3.0.1/String.html)
- [Reguläre Ausdrücke in Ruby](https://ruby-doc.org/core-3.0.1/Regexp.html)