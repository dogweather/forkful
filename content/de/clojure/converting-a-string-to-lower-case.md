---
title:    "Clojure: In eine Zeichenkette in Kleinbuchstaben umwandeln"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/clojure/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Warum

In der Programmierung ist es oft notwendig, Strings in Kleinbuchstaben zu konvertieren, sei es für Benutzereingaben, Datenbankabfragen oder generell für die Konsistenz der Daten. In diesem Blogbeitrag werden wir uns anschauen, wie man dies in Clojure umsetzen kann.

## Wie geht man vor

Um einen String in Kleinbuchstaben umzuwandeln, gibt es in Clojure mehrere Möglichkeiten. Hier sind zwei Beispiele:

```Clojure
;; Variante 1: Die Funktion "clojure.string/lower-case" verwenden
(clojure.string/lower-case "Hallo WELT") ; Output: "hallo welt"

;; Variante 2: Das "str" Makro und die Funktion "to-lower-case" kombinieren
(-> "GUTEN MORGEN" str clojure.string/to-lower-case) ; Output: "guten morgen"
```

Wie man sieht, ist es in Clojure sehr einfach, Strings in Kleinbuchstaben umzuwandeln. Das "clojure.string" Modul bietet bereits die nötigen Funktionen und Makros dafür.

## Tiefere Einblicke

Clojure behandelt Strings als unveränderliche Sequenzen von Zeichen, daher ist es wichtig, beim Arbeiten mit Strings darauf zu achten, dass sie nicht versehentlich verändert werden. Die Funktionen im "clojure.string" Modul geben immer eine neue String-Instanz zurück und ändern nicht den ursprünglichen String.

Es ist auch zu beachten, dass die Konvertierung von Groß- zu Kleinbuchstaben je nach Sprache unterschiedlich sein kann. Clojure nutzt die Standardbibliothek des Betriebssystems, um die Konvertierung korrekt durchzuführen.

## Siehe auch

- [Dokumentation für "clojure.string"](https://clojuredocs.org/clojure.string)
- [Beispiele für die Arbeit mit Strings in Clojure](https://www.baeldung.com/clojure-strings)