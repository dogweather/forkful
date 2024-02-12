---
title:                "Zeichenketten verknüpfen"
aliases: - /de/clojure/concatenating-strings.md
date:                  2024-01-20T17:34:19.896197-07:00
model:                 gpt-4-1106-preview
simple_title:         "Zeichenketten verknüpfen"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/concatenating-strings.md"
---

{{< edit_this_page >}}

## Was & Warum?
In Clojure und anderen Sprachen bedeutet das Verketten von Strings, dass wir sie aneinanderreihen, um einen neuen, längeren String zu bilden. Das ist nützlich, um Texte dynamisch zu generieren oder Daten zu formatieren.

## How to:
Hier sind ein paar Beispiele, wie man Strings in Clojure verketten kann:

```Clojure
;; Mit dem str-Befehl
(str "Hallo, " "Welt!")
;; => "Hallo, Welt!"

;; Mit dem `str`-Befehl und Variablen
(let [gruss "Hallo, " name "Welt!"]
  (str gruss name))
;; => "Hallo, Welt!"

;; Verkettung innerhalb von println
(println "Hallo, " "Welt!")
;; Ausgabe: Hallo, Welt!
```

## Deep Dive:
Das Verketten von Strings ist eine grundlegende Operation, die es seit den Anfängen der Programmierung gibt. In LISP-basierten Sprachen wie Clojure ist `str` eine eingebaute Funktion, die speziell für diese Aufgabe gedacht ist. Alternativ kann man auch `StringBuilder` aus Java benutzen, um String-Operationen effizienter zu gestalten, besonders bei großen oder vielen Strings. Im Kern ist das Verketten von Strings ein Prozess, bei dem mehrere Zeichenketten hintereinander in den Speicher geschrieben werden.

## See Also:
- Clojure's `str` Funktion: https://clojuredocs.org/clojure.core/str
- Java's `StringBuilder`: https://docs.oracle.com/javase/8/docs/api/java/lang/StringBuilder.html
- Clojure for the Brave and True (ein Buch für Clojure-Einsteiger): https://www.braveclojure.com/clojure-for-the-brave-and-true/
