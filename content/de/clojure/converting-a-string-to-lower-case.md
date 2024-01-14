---
title:                "Clojure: Umwandlung eines Strings in Kleinbuchstaben"
programming_language: "Clojure"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Warum String in Kleinbuchstaben konvertieren?

Das Konvertieren eines Strings in Kleinbuchstaben ist eine häufige Aufgabe in der Programmierung, die viele Vorteile bietet. Durch die Verwendung von Kleinbuchstaben in einem Text können wir zum Beispiel die Lesbarkeit verbessern und Texte leichter vergleichen.

## Wie man einen String in Kleinbuchstaben konvertiert

Das Konvertieren eines Strings in Kleinbuchstaben in Clojure ist ganz einfach. Verwenden Sie einfach die Funktion `lower-case`, um den Text zu konvertieren.

```Clojure
(lower-case "HELLO WORLD")
```

Output: "hello world"

Um sicherzustellen, dass alle Zeichen korrekt konvertiert werden, können wir auch die Funktion `normalize-string` verwenden.

```Clojure
(normalize-string (lower-case "HALLÖ") :form [:nfd])
```

Output: "hällo" 

## Tiefer Einblick

Bei der Konvertierung von Strings in Clojure ist es wichtig zu wissen, dass die Funktion `lower-case` keine Unterstützung für den Unicode-Lowercasing-Algorithmus bietet. Stattdessen verwendet es das Standardverfahren für die Umwandlung von ASCII-Zeichen, was zu unerwarteten Ergebnissen führen kann, wenn nicht-ASCII-Zeichen vorhanden sind.

Um dieses Problem zu umgehen, können wir die Funktion `normalize-string` verwenden, um zunächst die Zeichen in die normalisierte Form zu bringen und dann den Unicode-Lowercasing-Algorithmus anzuwenden.

## Siehe auch

- [ClojureString - lower-case](https://clojuredocs.org/clojure.string/lower-case)
- [Unicode Standard Annex #44 - Case Mappings](https://unicode.org/reports/tr44/)
- [Unicode Character Database](https://www.unicode.org/ucd/)